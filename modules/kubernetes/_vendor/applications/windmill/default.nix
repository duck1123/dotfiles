{ ... }:
{
  flake.nixidyApps.windmill =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      name = "windmill";
      db-password-secret = "windmill-database-password";
      shared-work-volume = "windmill-db-url-work";
      superadmin-secret = "windmill-superadmin-secret";
      secret-vars-secret = "windmill-secret-variables";
      connections-secret = "windmill-postgres-connections";

      # The declarative sync tooling -- the `wmill` CLI, its shell dependencies,
      # and applications/windmill/wmill/** itself -- is built as the
      # `windmill-sync-bundle` flake package (modules/pkgs/windmill-sync.nix,
      # symlinkJoin of wmill-cli + bash/curl/jq/coreutils + the wmill/ config
      # tree at share/windmill-wmill). Its output store path is resolved right
      # here at `nur switch` time and passed to nix-csi via the CSI driver's
      # per-system storePath convention (volumeAttributes keyed by Nix system
      # string, e.g. "x86_64-linux") rather than a nixExpr string -- same
      # pattern and same reason as applications/duck1123/default.nix's
      # duck1123Runtime: nix-csi evaluates nixExpr without --impure, and
      # builtins.storePath is rejected in pure eval, so embedding the literal
      # path inside nixExpr's source text hard-fails NodePublishVolume. There's
      # no fallback: this exact path must be pushed to Attic (`nur switch` does
      # this automatically via `nur push-site-cache windmill-sync-bundle`,
      # scripts/nur.nu) as part of every switch that changes wmill/**, or
      # nix-csi has nothing to substitute it from.
      windmillSyncBundle = self.packages.x86_64-linux.windmill-sync-bundle;

      # Small toolset (bash/coreutils/git/curl/jq/nushell/nix) mounted at /nix
      # inside the ${name}-worker-native container so "native"-tagged Windmill
      # scripts have something to run beyond the windmill-labs image itself --
      # built as the `windmill-worker-native-tools` flake package
      # (modules/pkgs/windmill-sync.nix) and resolved via the same
      # storePath convention as windmillSyncBundle above, rather than a
      # nixExpr string nix-csi would have to fetch nixpkgs and re-evaluate
      # for on every mount (the pattern applications/xysat.nix uses instead,
      # justified there by per-node customization -- not needed here since
      # this toolset is fixed).
      windmillWorkerTools = self.packages.x86_64-linux.windmill-worker-native-tools;

      # The NAS (config.nfsTargets.nas, 192.168.0.124:/volume1) only grants
      # NFS access per top-level shared folder -- /volume1 itself is denied
      # by the server ("access denied by server while mounting
      # 192.168.0.124:/volume1"), which is what a single whole-basePath mount
      # hit here previously. Every other app on this NAS works around the
      # same restriction by mounting one specific child folder per
      # PV/PVC (see applications/radarr.nix, mediamanager.nix); this mounts
      # every such folder individually so windmill workers see the same
      # library any *arr app does, none of it as a single denied root mount.
      mediaFolders = [
        "Downloads"
        "Movies"
        "TV"
        "Music"
        "Podcasts"
        "Youtube"
        "YT-Cache"
        "Videos"
        "Books"
        "slskd_downloads"
      ];
      mediaSlug = folder: builtins.replaceStrings [ "_" ] [ "-" ] (lib.toLower folder);
      mediaVolumeName = folder: "media-${mediaSlug folder}";
      mediaPvcName = folder: "${name}-${name}-media-${mediaSlug folder}";
      mediaPvName = folder: "${mediaPvcName folder}-nfs";

      mediaVolumes =
        cfg:
        lib.optionals cfg.nfs.enable (
          map (folder: {
            name = mediaVolumeName folder;
            persistentVolumeClaim.claimName = mediaPvcName folder;
          }) mediaFolders
        );

      mediaVolumeMounts =
        cfg:
        lib.optionals cfg.nfs.enable (
          map (folder: {
            mountPath = "/media/${folder}";
            name = mediaVolumeName folder;
          }) mediaFolders
        );

      # Runs on every ArgoCD sync (see the Job's hook annotations below), so
      # editing applications/windmill/wmill/** and pushing is the only step
      # needed to change scripts/flows/apps/resources/variables -- same "one
      # build/push" loop as everything else in this repo. Secret variable
      # *values* never live in git (wmill.yaml: skipSecrets) so they're seeded
      # here from cfg.secretVariables (sops) before the sync runs.
      syncScript = cfg: ''
        set -euo pipefail

        base_url="http://${name}.${name}:${toString cfg.service.port}"

        echo "Waiting for Windmill to become healthy..."
        health_timeout=180
        health_elapsed=0
        until curl -sf "$base_url/healthz" >/dev/null; do
          if [ "$health_elapsed" -ge "$health_timeout" ]; then
            echo "Windmill did not become healthy within ''${health_timeout}s" >&2
            exit 1
          fi
          sleep 3
          health_elapsed=$((health_elapsed + 3))
        done

        export HOME=/tmp

        # Separate from SUPERADMIN_SECRET (which only authenticates *this job's*
        # API/CLI calls) -- this is a real, loginable instance user, so you can
        # sign into the Windmill UI yourself. `wmill user add` isn't documented
        # as idempotent, so a failure here is treated as "probably already
        # exists" and logged rather than failing the job.
        if [ -n "''${SUPERADMIN_EMAIL:-}" ] && [ -n "''${SUPERADMIN_PASSWORD:-}" ]; then
          echo "Ensuring Windmill superadmin user \"$SUPERADMIN_EMAIL\" exists..."
          if ! wmill user add "$SUPERADMIN_EMAIL" "$SUPERADMIN_PASSWORD" --superadmin \
              --token "$SUPERADMIN_SECRET" --base-url "$base_url" --workspace "$WORKSPACE" \
              --config-dir /tmp/wmill-config 2>/tmp/user-add.log; then
            echo "wmill user add did not succeed (likely already exists), continuing:" >&2
            cat /tmp/user-add.log >&2
          fi
        fi

        count=$(echo "''${SECRET_VARIABLES_JSON:-[]}" | jq 'length')
        for i in $(seq 0 $((count - 1))); do
          var=$(echo "$SECRET_VARIABLES_JSON" | jq -c ".[$i]")
          var_path=$(echo "$var" | jq -r '.path')
          var_value=$(echo "$var" | jq -r '.value')

          body=$(jq -n --arg path "$var_path" --arg value "$var_value" \
            '{path: $path, value: $value, is_secret: true, description: ""}')

          status=$(curl -s -o /tmp/resp -w '%{http_code}' -X POST \
            "$base_url/api/w/$WORKSPACE/variables/create" \
            -H "Authorization: Bearer $SUPERADMIN_SECRET" \
            -H "Content-Type: application/json" -d "$body")

          if [ "$status" = "409" ]; then
            echo "Updating existing Windmill variable \"$var_path\""
            curl -sf -X POST "$base_url/api/w/$WORKSPACE/variables/update/$var_path" \
              -H "Authorization: Bearer $SUPERADMIN_SECRET" \
              -H "Content-Type: application/json" \
              -d "$(jq -n --arg value "$var_value" '{value: $value}')" >/dev/null
          elif [ "''${status:0:1}" != "2" ]; then
            echo "Failed to create Windmill variable \"$var_path\" (HTTP $status)" >&2
            cat /tmp/resp >&2
            exit 1
          else
            echo "Created Windmill variable \"$var_path\""
          fi
        done

        echo "Pushing declarative config from applications/windmill/wmill ..."
        cd /nix/var/result/share/windmill-wmill
        wmill sync push \
          --workspace "$WORKSPACE" \
          --token "$SUPERADMIN_SECRET" \
          --base-url "$base_url" \
          --config-dir /tmp/wmill-config \
          --yes

        # Runs *after* wmill sync push, not before: push treats
        # applications/windmill/wmill/f/** as authoritative for the whole f/
        # folder and deletes any remote resource under it that isn't checked
        # into git -- which is everywhere db_<name> resources live, since
        # their values are real per-app secrets that can't round-trip through
        # git the way example_postgres.resource.yaml's placeholder can.
        # Registering them after push means each run's leftovers from the
        # *previous* run get pruned by push and then immediately recreated
        # here, rather than deleted and left gone.
        conn_count=$(echo "''${CONNECTIONS_JSON:-[]}" | jq 'length')
        for i in $(seq 0 $((conn_count - 1))); do
          conn=$(echo "$CONNECTIONS_JSON" | jq -c ".[$i]")
          conn_name=$(echo "$conn" | jq -r '.name')
          conn_password=$(echo "$conn" | jq -r '.password')
          resource_path="f/fleetops/db_$conn_name"
          password_var_path="f/fleetops/db_''${conn_name}_password"

          # The password is stored as its own secret Windmill variable
          # (encrypted at rest by Windmill) and the resource references it
          # via $var:, exactly like the checked-in
          # example_postgres.resource.yaml / _password.variable.yaml pair --
          # never as a raw string in the resource's own value, which
          # round-trips through plain (unencrypted-by-Windmill) resource
          # storage and export.
          var_body=$(jq -n --arg path "$password_var_path" --arg value "$conn_password" \
            --arg desc "Password for $resource_path.resource.yaml, managed by the windmill-sync ArgoCD hook job." \
            '{path: $path, value: $value, is_secret: true, description: $desc}')

          var_status=$(curl -s -o /tmp/resp -w '%{http_code}' -X POST \
            "$base_url/api/w/$WORKSPACE/variables/create" \
            -H "Authorization: Bearer $SUPERADMIN_SECRET" \
            -H "Content-Type: application/json" -d "$var_body")

          if [ "$var_status" = "409" ]; then
            curl -sf -X POST "$base_url/api/w/$WORKSPACE/variables/update/$password_var_path" \
              -H "Authorization: Bearer $SUPERADMIN_SECRET" \
              -H "Content-Type: application/json" \
              -d "$(jq -n --arg value "$conn_password" '{value: $value}')" >/dev/null
          elif [ "''${var_status:0:1}" != "2" ]; then
            echo "Failed to create Windmill variable \"$password_var_path\" (HTTP $var_status)" >&2
            cat /tmp/resp >&2
            exit 1
          fi

          # update_if_exists means one call handles both create and update --
          # no separate existence check needed (unlike superset/metabase,
          # which register connections through their own app APIs instead of
          # Windmill's).
          body=$(echo "$conn" | jq \
            --arg path "$resource_path" \
            --arg desc "Reporting connection for the $conn_name database on the shared postgresql instance -- managed by the windmill-sync ArgoCD hook job, sourced from env/dev/windmill.nix's reportingConnections." \
            --arg password_ref "\$var:$password_var_path" \
            '{path: $path, resource_type: "postgresql", description: $desc, value: {host: .host, port: .port, user: .username, dbname: .name, sslmode: "disable", password: $password_ref}}')

          echo "Registering Windmill resource \"$resource_path\""
          curl -sf -X POST "$base_url/api/w/$WORKSPACE/resources/create?update_if_exists=true" \
            -H "Authorization: Bearer $SUPERADMIN_SECRET" \
            -H "Content-Type: application/json" \
            -d "$body" >/dev/null
        done
      '';
    in
    self.lib.mkArgoApp
      {
        inherit
          config
          lib
          self
          pkgs
          ;
      }
      rec {
        inherit name;
        uses-ingress = true;
        uses-database = true;
        # nfsSubPath left at its "" default, so cfg.nfs.path resolves to the
        # whole nfsTarget basePath (all of nasnix:/mnt/media) rather than one
        # category subfolder -- windmill workers get the same media library
        # every *arr app sees, not a curated slice of it.
        uses-nfs = true;

        # Store only the raw password; init container builds DATABASE_URL at runtime with proper URL encoding.
        sopsSecrets =
          cfg:
          lib.optionalAttrs (cfg.database.password != "") {
            ${db-password-secret} = {
              password = cfg.database.password;
            };
          }
          // lib.optionalAttrs (cfg.superadminSecret != "") {
            ${superadmin-secret} = {
              SUPERADMIN_SECRET = cfg.superadminSecret;
            }
            // lib.optionalAttrs (cfg.superadminEmail != "" && cfg.superadminPassword != "") {
              SUPERADMIN_EMAIL = cfg.superadminEmail;
              SUPERADMIN_PASSWORD = cfg.superadminPassword;
            };
          }
          // lib.optionalAttrs (cfg.secretVariables != [ ]) {
            ${secret-vars-secret} = {
              SECRET_VARIABLES_JSON = builtins.toJSON (
                map (v: { inherit (v) path value; }) cfg.secretVariables
              );
            };
          }
          // lib.optionalAttrs (cfg.reportingConnections != [ ]) {
            ${connections-secret} = {
              CONNECTIONS_JSON = builtins.toJSON (
                map (c: {
                  inherit (c) name host port username password;
                }) cfg.reportingConnections
              );
            };
          };

        extraOptions = {
          image = mkOption {
            description = mdDoc "The Windmill docker image";
            type = types.str;
            default = "ghcr.io/windmill-labs/windmill:latest";
          };

          service.port = mkOption {
            description = mdDoc "The service port";
            type = types.int;
            default = 8000;
          };

          replicas = mkOption {
            description = mdDoc "Number of Windmill replicas";
            type = types.int;
            default = 1;
          };

          workspace = mkOption {
            description = mdDoc "Windmill workspace id that applications/windmill/wmill/** is synced into.";
            type = types.str;
            default = "default";
          };

          superadminSecret = mkOption {
            description = mdDoc ''
              Bearer token that authenticates as a Windmill superadmin. Set as the
              server's SUPERADMIN_SECRET env var; per Windmill's docs, any request
              presenting this exact string as a Bearer token is authenticated as
              superadmin_secret@windmill.dev with full admin rights -- no
              login/setup-token dance needed (unlike Metabase's admin.password).
              Stored in secrets.enc.yaml as `windmill.superadminSecret` (generate:
              `openssl rand -hex 20`). Left empty, the windmill-sync job is skipped
              entirely, since there'd be no way for it to authenticate.
            '';
            type = types.str;
            default = "";
          };

          superadminEmail = mkOption {
            description = mdDoc ''
              Email for a real, loginable Windmill superadmin account -- separate
              from superadminSecret, which only authenticates the windmill-sync
              job's own API/CLI calls and isn't a user you can sign into the UI
              with. Created (idempotently best-effort, via `wmill user add
              --superadmin`) by the windmill-sync job whenever this and
              superadminPassword are both set. Stored in secrets.enc.yaml as
              `windmill.superadminEmail`.
            '';
            type = types.str;
            default = "";
          };

          superadminPassword = mkOption {
            description = mdDoc ''
              Password for the superadminEmail account. Stored in
              secrets.enc.yaml as `windmill.superadminPassword`.
            '';
            type = types.str;
            default = "";
          };

          secretVariables = mkOption {
            description = mdDoc ''
              Windmill secret variables to seed via the REST API before each
              `wmill sync push` -- the CLI never pushes secret variable *values*
              (see applications/windmill/wmill/wmill.yaml's skipSecrets), so any
              password a checked-in resource references via `$var:<path>` has to
              land here instead. See
              applications/windmill/wmill/f/fleetops/*.variable.yaml for the
              corresponding (valueless) definitions checked into git.
            '';
            type = types.listOf (
              types.submodule {
                options = {
                  path = mkOption {
                    type = types.str;
                    description = mdDoc "Windmill variable path, e.g. f/fleetops/example_postgres_password.";
                  };
                  value = mkOption {
                    type = types.str;
                    description = mdDoc "Secret value.";
                  };
                };
              }
            );
            default = [ ];
          };

          reportingConnections = mkOption {
            description = mdDoc ''
              Postgres databases to register as Windmill `postgresql` resources, one
              entry per database (Postgres has no cross-database queries, so a single
              connection can't cover multiple databases on the same instance).
              Registered declaratively by the windmill-sync job, which calls
              Windmill's REST API (POST /resources/create?update_if_exists=true)
              since these are real per-app databases, unlike the static example
              checked in at wmill/f/fleetops/example_postgres.resource.yaml -- see
              env/dev/windmill.nix, which populates this from
              config.services.postgresql.extraDatabases so it stays in sync with
              that list automatically. Each entry should use that database's own
              least-privilege role rather than the Postgres admin role. Registered
              at path f/fleetops/db_<name>.
            '';
            type = types.listOf (
              types.submodule {
                options = {
                  name = mkOption {
                    type = types.str;
                    description = mdDoc "Database name -- also used as the resource's dbname and the db_<name> path suffix.";
                  };
                  host = mkOption {
                    type = types.str;
                    description = mdDoc "Postgres host.";
                  };
                  port = mkOption {
                    type = types.port;
                    description = mdDoc "Postgres port.";
                  };
                  username = mkOption {
                    type = types.str;
                    description = mdDoc "Role to connect as.";
                  };
                  password = mkOption {
                    type = types.str;
                    description = mdDoc "Password for that role.";
                  };
                };
              }
            );
            default = [ ];
          };
        };

        extraResources = cfg: {
          deployments = {
            "${name}-worker-native" = {
              metadata.labels = {
                "app.kubernetes.io/instance" = "${name}-worker-native";
                "app.kubernetes.io/name" = "${name}-worker-native";
              };

              spec = {
                replicas = 1;
                selector.matchLabels = {
                  "app.kubernetes.io/instance" = "${name}-worker-native";
                  "app.kubernetes.io/name" = "${name}-worker-native";
                };

                template = {
                  metadata.labels = {
                    "app.kubernetes.io/instance" = "${name}-worker-native";
                    "app.kubernetes.io/name" = "${name}-worker-native";
                  };

                  spec = {
                    automountServiceAccountToken = true;
                    serviceAccountName = "default";

                    initContainers = lib.optionals (cfg.database.password != "") [
                      {
                        name = "build-database-url";
                        image = "python:3-alpine";
                        imagePullPolicy = "IfNotPresent";
                        command = [
                          "python3"
                          "-c"
                          ''
                            import urllib.parse
                            import os
                            user = os.environ["PGUSER"]
                            password = os.environ["PGPASSWORD"]
                            host = os.environ["PGHOST"]
                            port = os.environ["PGPORT"]
                            db = os.environ["PGDATABASE"]
                            enc = urllib.parse.quote(password, safe="")
                            url = f"postgresql://{user}:{enc}@{host}:{port}/{db}?sslmode=disable"
                            with open("/work/database_url", "w") as f:
                                f.write(url)
                          ''
                        ];
                        env = [
                          {
                            name = "PGUSER";
                            value = cfg.database.username;
                          }
                          {
                            name = "PGHOST";
                            value = cfg.database.host;
                          }
                          {
                            name = "PGPORT";
                            value = toString cfg.database.port;
                          }
                          {
                            name = "PGDATABASE";
                            value = cfg.database.name;
                          }
                          {
                            name = "PGPASSWORD";
                            valueFrom.secretKeyRef = {
                              name = db-password-secret;
                              key = "password";
                            };
                          }
                        ];
                        volumeMounts = [
                          {
                            mountPath = "/work";
                            name = shared-work-volume;
                          }
                        ];
                      }
                    ];

                    containers = [
                      (
                        {
                          name = "${name}-worker-native";
                          image = cfg.image;
                          imagePullPolicy = "IfNotPresent";
                          env = [
                            {
                              name = "TZ";
                              value = cfg.tz;
                            }
                            {
                              name = "MODE";
                              value = "worker";
                            }
                            {
                              name = "WORKER_GROUP";
                              value = "native";
                            }
                            {
                              name = "WORKER_TAGS";
                              value = "native";
                            }
                            {
                              name = "PATH";
                              value = "/nix/var/result/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin";
                            }
                            {
                              # See applications/xysat.nix for why store/sandbox/build-users-group
                              # are set this way -- same rationale applies here.
                              name = "NIX_CONFIG";
                              value = ''
                                experimental-features = nix-command flakes
                                extra-substituters = https://attic.home.kronkltd.net/nixos
                                extra-trusted-public-keys = nixos:/5T+7JIEApx8OL/j4HhK1koV6jMPu3rZV098GsuBAi4=
                                store = local?root=/var/lib/nix-scratch
                                sandbox = false
                                build-users-group =
                              '';
                            }
                          ];
                        }
                        // lib.optionalAttrs (cfg.database.password != "") {
                          command = [
                            "/bin/sh"
                            "-c"
                            "export DATABASE_URL=$(cat /work/database_url) && exec windmill"
                          ];
                        }
                        // {
                          volumeMounts =
                            lib.optionals (cfg.database.password != "") [
                              {
                                mountPath = "/work";
                                name = shared-work-volume;
                              }
                            ]
                            ++ [
                              {
                                mountPath = "/nix";
                                name = "nix";
                                subPath = "nix";
                              }
                              {
                                mountPath = "/var/lib/nix-scratch";
                                name = "nix-scratch";
                              }
                            ]
                            ++ mediaVolumeMounts cfg;
                        }
                      )
                    ];

                    volumes = lib.optionals (cfg.database.password != "") [
                      {
                        name = shared-work-volume;
                        emptyDir = { };
                      }
                    ]
                    ++ [
                      {
                        name = "nix";
                        csi = {
                          driver = "nix.csi.store";
                          volumeAttributes."x86_64-linux" = "${windmillWorkerTools}";
                        };
                      }
                      {
                        # Writable, node-local, wiped on pod restart -- see the
                        # NIX_CONFIG `store` setting above.
                        name = "nix-scratch";
                        emptyDir = { };
                      }
                    ]
                    ++ mediaVolumes cfg;
                  };
                };
              };
            };

            ${name} = {
              metadata.labels = {
                "app.kubernetes.io/instance" = name;
                "app.kubernetes.io/name" = name;
                "app.kubernetes.io/version" = "latest";
              };

              spec = {
                replicas = cfg.replicas;
                selector.matchLabels = {
                  "app.kubernetes.io/instance" = name;
                  "app.kubernetes.io/name" = name;
                };

                template = {
                  metadata.labels = {
                    "app.kubernetes.io/instance" = name;
                    "app.kubernetes.io/name" = name;
                  };

                  spec = {
                    automountServiceAccountToken = true;
                    serviceAccountName = "default";

                    # Build DATABASE_URL at runtime with proper URL encoding (handles special chars in password).
                    initContainers = lib.optionals (cfg.database.password != "") [
                      {
                        name = "build-database-url";
                        image = "python:3-alpine";
                        imagePullPolicy = "IfNotPresent";
                        command = [
                          "python3"
                          "-c"
                          ''
                            import urllib.parse
                            import os
                            user = os.environ["PGUSER"]
                            password = os.environ["PGPASSWORD"]
                            host = os.environ["PGHOST"]
                            port = os.environ["PGPORT"]
                            db = os.environ["PGDATABASE"]
                            enc = urllib.parse.quote(password, safe="")
                            url = f"postgresql://{user}:{enc}@{host}:{port}/{db}?sslmode=disable"
                            with open("/work/database_url", "w") as f:
                                f.write(url)
                          ''
                        ];
                        env = [
                          {
                            name = "PGUSER";
                            value = cfg.database.username;
                          }
                          {
                            name = "PGHOST";
                            value = cfg.database.host;
                          }
                          {
                            name = "PGPORT";
                            value = toString cfg.database.port;
                          }
                          {
                            name = "PGDATABASE";
                            value = cfg.database.name;
                          }
                          {
                            name = "PGPASSWORD";
                            valueFrom.secretKeyRef = {
                              name = db-password-secret;
                              key = "password";
                            };
                          }
                        ];
                        volumeMounts = [
                          {
                            mountPath = "/work";
                            name = shared-work-volume;
                          }
                        ];
                      }
                    ];

                    containers = [
                      (
                        {
                          inherit name;
                          image = cfg.image;
                          imagePullPolicy = "IfNotPresent";
                          env = [
                            {
                              name = "TZ";
                              value = cfg.tz;
                            }
                            {
                              name = "MODE";
                              value = "standalone";
                            }
                            {
                              name = "BASE_URL";
                              value = "https://${cfg.ingress.domain}";
                            }
                            {
                              name = "WORKER_TAGS";
                              value = "deno,python3,bash,go,dependency,flow,hub";
                            }
                            {
                              # Gives this pod's own worker (the one that actually
                              # runs bash/nu-tagged jobs, unlike windmill-worker-native
                              # below which Windmill forces into a fixed "native jobs
                              # only" mode -- see the PATH-prepending command below for
                              # why nix isn't just added to PATH here directly) access
                              # to `nix`. See applications/xysat.nix for why
                              # store/sandbox/build-users-group are set this way.
                              name = "NIX_CONFIG";
                              value = ''
                                experimental-features = nix-command flakes
                                extra-substituters = https://attic.home.kronkltd.net/nixos
                                extra-trusted-public-keys = nixos:/5T+7JIEApx8OL/j4HhK1koV6jMPu3rZV098GsuBAi4=
                                store = local?root=/var/lib/nix-scratch
                                sandbox = false
                                build-users-group =
                              '';
                            }
                          ]
                          ++ lib.optionals (cfg.superadminSecret != "") [
                            {
                              name = "SUPERADMIN_SECRET";
                              valueFrom.secretKeyRef = {
                                name = superadmin-secret;
                                key = "SUPERADMIN_SECRET";
                              };
                            }
                          ];
                          ports = [
                            {
                              containerPort = cfg.service.port;
                              name = "http";
                              protocol = "TCP";
                            }
                          ];
                          readinessProbe = {
                            httpGet = {
                              path = "/healthz";
                              port = cfg.service.port;
                            };
                            initialDelaySeconds = 20;
                            periodSeconds = 10;
                            timeoutSeconds = 5;
                            successThreshold = 1;
                            failureThreshold = 5;
                          };
                          livenessProbe = {
                            httpGet = {
                              path = "/healthz";
                              port = cfg.service.port;
                            };
                            initialDelaySeconds = 40;
                            periodSeconds = 30;
                            timeoutSeconds = 5;
                            successThreshold = 1;
                            failureThreshold = 5;
                          };
                        }
                        // lib.optionalAttrs (cfg.database.password != "") {
                          command = [
                            "/bin/sh"
                            "-c"
                            # Prepend rather than replace PATH -- unlike
                            # windmill-worker-native's container (which has no other
                            # runtime to preserve), this pod's default PATH still needs
                            # to resolve the image's own bundled deno/python3/go/etc.
                            "export DATABASE_URL=$(cat /work/database_url) && export PATH=\"/nix/var/result/bin:$PATH\" && exec windmill standalone"
                          ];
                          volumeMounts = [
                            {
                              mountPath = "/work";
                              name = shared-work-volume;
                            }
                            {
                              mountPath = "/nix";
                              name = "nix";
                              subPath = "nix";
                            }
                            {
                              mountPath = "/var/lib/nix-scratch";
                              name = "nix-scratch";
                            }
                          ]
                          ++ mediaVolumeMounts cfg;
                        }
                      )
                    ];

                    volumes = lib.optionals (cfg.database.password != "") [
                      {
                        name = shared-work-volume;
                        emptyDir = { };
                      }
                      {
                        name = "nix";
                        csi = {
                          driver = "nix.csi.store";
                          volumeAttributes."x86_64-linux" = "${windmillWorkerTools}";
                        };
                      }
                      {
                        name = "nix-scratch";
                        emptyDir = { };
                      }
                    ]
                    ++ mediaVolumes cfg;
                  };
                };
              };
            };
          };

          ingresses.${name} = with cfg.ingress; {
            metadata.annotations."cert-manager.io/cluster-issuer" = clusterIssuer;
            spec = {
              inherit ingressClassName;

              rules = [
                {
                  host = domain;

                  http.paths = [
                    {
                      backend.service = {
                        inherit name;
                        port.name = "http";
                      };

                      path = "/";
                      pathType = "ImplementationSpecific";
                    }
                  ];
                }
              ];

              tls = [
                {
                  hosts = [ domain ];
                  secretName = "${name}-tls";
                }
              ];
            };
          };

          services.${name}.spec = {
            ports = [
              {
                name = "http";
                port = cfg.service.port;
                protocol = "TCP";
                targetPort = "http";
              }
            ];

            selector = {
              "app.kubernetes.io/instance" = name;
              "app.kubernetes.io/name" = name;
            };

            type = "ClusterIP";
          };
        }
        // lib.optionalAttrs cfg.nfs.enable {
          persistentVolumes = lib.listToAttrs (
            map (folder: {
              name = mediaPvName folder;
              value = {
                apiVersion = "v1";
                kind = "PersistentVolume";
                metadata.name = mediaPvName folder;
                spec = {
                  capacity.storage = "1Ti";
                  accessModes = [ "ReadWriteMany" ];
                  mountOptions = [
                    "nolock"
                    "noexec"
                    "soft"
                    "timeo=30"
                  ];
                  nfs = {
                    server = cfg.nfs.server;
                    path = "${cfg.nfs.path}/${folder}";
                  };
                  persistentVolumeReclaimPolicy = "Retain";
                };
              };
            }) mediaFolders
          );

          persistentVolumeClaims = lib.listToAttrs (
            map (folder: {
              name = mediaPvcName folder;
              value.spec = {
                accessModes = [ "ReadWriteMany" ];
                resources.requests.storage = "1Gi";
                storageClassName = "";
                volumeName = mediaPvName folder;
              };
            }) mediaFolders
          );
        }
        // lib.optionalAttrs (cfg.superadminSecret != "") {
          jobs."${name}-sync" = {
            metadata.annotations = {
              "argocd.argoproj.io/hook" = "Sync";
              "argocd.argoproj.io/hook-delete-policy" = "BeforeHookCreation,HookSucceeded";
              # Runs after the chart's own Deployment (wave "0", implicit) is
              # created -- the job polls /healthz so it tolerates the pod
              # not being Ready yet, it just needs the Service to exist.
              "argocd.argoproj.io/sync-wave" = "1";
            };
            spec = {
              backoffLimit = 3;
              template.spec = {
                restartPolicy = "OnFailure";
                containers = [
                  {
                    name = "windmill-sync";
                    image = "ghcr.io/lillecarl/nix-csi/scratch:1.0.1";
                    command = [
                      "bash"
                      "-c"
                      (syncScript cfg)
                    ];
                    env = [
                      {
                        name = "WORKSPACE";
                        value = cfg.workspace;
                      }
                    ];
                    envFrom = [
                      { secretRef.name = superadmin-secret; }
                    ]
                    ++ lib.optionals (cfg.secretVariables != [ ]) [
                      { secretRef.name = secret-vars-secret; }
                    ]
                    ++ lib.optionals (cfg.reportingConnections != [ ]) [
                      { secretRef.name = connections-secret; }
                    ];
                    volumeMounts = [
                      {
                        name = "nix";
                        mountPath = "/nix";
                        subPath = "nix";
                      }
                      {
                        name = "tmp";
                        mountPath = "/tmp";
                      }
                    ];
                  }
                ];
                volumes = [
                  {
                    name = "nix";
                    csi = {
                      driver = "nix.csi.store";
                      volumeAttributes."x86_64-linux" = "${windmillSyncBundle}";
                    };
                  }
                  # The scratch image has no /tmp of its own -- the script sets
                  # HOME=/tmp and writes a log file and wmill's --config-dir
                  # there, all of which need a writable directory to land in.
                  {
                    name = "tmp";
                    emptyDir = { };
                  }
                ];
              };
            };
          };
        };
      };
}
