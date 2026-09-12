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
                          ];
                        }
                        // lib.optionalAttrs (cfg.database.password != "") {
                          command = [
                            "/bin/sh"
                            "-c"
                            "export DATABASE_URL=$(cat /work/database_url) && exec windmill"
                          ];
                          volumeMounts = [
                            {
                              mountPath = "/work";
                              name = shared-work-volume;
                            }
                          ];
                        }
                      )
                    ];

                    volumes = lib.optionals (cfg.database.password != "") [
                      {
                        name = shared-work-volume;
                        emptyDir = { };
                      }
                    ];
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
                            "export DATABASE_URL=$(cat /work/database_url) && exec windmill standalone"
                          ];
                          volumeMounts = [
                            {
                              mountPath = "/work";
                              name = shared-work-volume;
                            }
                          ];
                        }
                      )
                    ];

                    volumes = lib.optionals (cfg.database.password != "") [
                      {
                        name = shared-work-volume;
                        emptyDir = { };
                      }
                    ];
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
