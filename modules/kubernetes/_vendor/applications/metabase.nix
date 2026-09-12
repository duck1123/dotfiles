{ ... }:
{
  flake.nixidyApps.metabase =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      name = "metabase";
      admin-secret = "metabase-admin";
      connections-secret = "metabase-postgres-connections";

      # nix-csi runtime for the register-connections job: Metabase's own image has
      # neither curl nor jq, and there's no Dockerfile of ours to add them to, so
      # bundle both (plus bash) the same way applications/demo.nix bundles python3.
      toolsExpr = ''
        let
          pkgs = import (builtins.fetchTree {
            type = "github";
            owner = "nixos";
            repo = "nixpkgs";
            ref = "nixos-unstable";
          }) {};
        in
        pkgs.symlinkJoin {
          name = "metabase-register-connections-tools";
          paths = [ pkgs.bash pkgs.curl pkgs.jq pkgs.coreutils ];
        }
      '';

      registerConnectionsScript = ''
        set -euo pipefail

        base_url="http://${name}.${name}"

        echo "Waiting for Metabase to become healthy..."
        until curl -sf "$base_url/api/health" >/dev/null; do
          sleep 3
        done

        props=$(curl -sf "$base_url/api/session/properties")
        has_user_setup=$(echo "$props" | jq -r '."has-user-setup"')

        if [ "$has_user_setup" != "true" ]; then
          echo "No admin exists yet -- completing first-run setup"
          setup_token=$(echo "$props" | jq -r '."setup-token"')
          setup_body=$(jq -n \
            --arg token "$setup_token" \
            --arg first_name "$FIRST_NAME" \
            --arg last_name "$LAST_NAME" \
            --arg email "$EMAIL" \
            --arg password "$PASSWORD" \
            '{token:$token,user:{first_name:$first_name,last_name:$last_name,email:$email,password:$password},prefs:{site_name:"Metabase",allow_tracking:false}}')
          session=$(curl -sf -X POST "$base_url/api/setup" \
            -H "Content-Type: application/json" -d "$setup_body" | jq -r '.id')
        else
          echo "Logging in as existing admin"
          login_body=$(jq -n --arg email "$EMAIL" --arg password "$PASSWORD" \
            '{username:$email,password:$password}')
          session=$(curl -sf -X POST "$base_url/api/session" \
            -H "Content-Type: application/json" -d "$login_body" | jq -r '.id')
        fi

        if [ -z "$session" ] || [ "$session" = "null" ]; then
          echo "Failed to obtain a Metabase session" >&2
          exit 1
        fi

        existing=$(curl -sf -H "X-Metabase-Session: $session" "$base_url/api/database")

        count=$(echo "$CONNECTIONS_JSON" | jq 'length')
        for i in $(seq 0 $((count - 1))); do
          conn=$(echo "$CONNECTIONS_JSON" | jq -c ".[$i]")
          conn_name=$(echo "$conn" | jq -r '.name')

          existing_id=$(echo "$existing" | jq -r --arg n "$conn_name" \
            '(.data // .) | .[]? | select(.name == $n) | .id' | head -n1)

          body=$(echo "$conn" | jq \
            '{engine: "postgres", name: .name, details: {host: .host, port: .port, dbname: .name, user: .username, password: .password, ssl: false, "tunnel-enabled": false}}')

          if [ -n "$existing_id" ]; then
            echo "Updating Metabase database \"$conn_name\" (id=$existing_id)"
            curl -sf -X PUT "$base_url/api/database/$existing_id" \
              -H "X-Metabase-Session: $session" -H "Content-Type: application/json" \
              -d "$body" >/dev/null
          else
            echo "Creating Metabase database \"$conn_name\""
            curl -sf -X POST "$base_url/api/database" \
              -H "X-Metabase-Session: $session" -H "Content-Type: application/json" \
              -d "$body" >/dev/null
          fi
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
      {
        inherit name;

        # https://artifacthub.io/packages/helm/pmint93/metabase
        chart = lib.helm.downloadHelmChart {
          repo = "https://pmint93.github.io/helm-charts";
          chart = "metabase";
          version = "2.27.6";
          chartHash = "sha256-9QwOq5ivvOvagEu99HZYjCRnakfv7aF9s+lY4Uho3/w=";
        };

        uses-ingress = true;

        extraOptions = {
          admin = {
            firstName = mkOption {
              description = mdDoc "First name for the initial Metabase admin account.";
              type = types.str;
              default = "Admin";
            };
            lastName = mkOption {
              description = mdDoc "Last name for the initial Metabase admin account.";
              type = types.str;
              default = "Admin";
            };
            email = mkOption {
              description = mdDoc "Email (also the login) for the initial Metabase admin account.";
              type = types.str;
              default = "admin@metabase.local";
            };
            password = mkOption {
              description = mdDoc ''
                Password for the initial Metabase admin account (generate: `openssl rand
                -hex 20`). Stored in secrets.enc.yaml as `metabase.admin.password`. Left
                empty, the metabase-register-connections job is skipped entirely --
                Metabase has no env-var/config-file mechanism for data source
                connections (unlike its own application database), so this password
                doubles as the credential the job uses to complete first-run setup (or
                log in as an already-set-up admin) before calling the REST API.
              '';
              type = types.str;
              default = "";
            };
          };

          reportingConnections = mkOption {
            description = mdDoc ''
              Postgres databases to register as Metabase data source connections, one
              entry per database. Registered declaratively by the
              metabase-register-connections job, which calls Metabase's REST API
              (POST/PUT /api/database) since there's no declarative config path for
              this -- see env/dev/metabase.nix.
            '';
            type = types.listOf (
              types.submodule {
                options = {
                  name = mkOption {
                    type = types.str;
                    description = mdDoc "Database name -- also used as the Metabase connection display name.";
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

        sopsSecrets =
          cfg:
          optionalAttrs (cfg.admin.password != "") {
            ${admin-secret} = {
              FIRST_NAME = cfg.admin.firstName;
              LAST_NAME = cfg.admin.lastName;
              EMAIL = cfg.admin.email;
              PASSWORD = cfg.admin.password;
            };
          }
          // optionalAttrs (cfg.reportingConnections != [ ]) {
            ${connections-secret} = {
              CONNECTIONS_JSON = builtins.toJSON (
                map (c: {
                  inherit (c) name host port username password;
                }) cfg.reportingConnections
              );
            };
          };

        defaultValues = cfg: {
          ingress = with cfg.ingress; {
            annotations = {
              "cert-manager.io/cluster-issuer" = clusterIssuer;
              "ingress.kubernetes.io/force-ssl-redirect" = "true";
            };
            className = ingressClassName;
            enabled = true;
            hosts = [ domain ];
            tls = [
              {
                secretName = "metabase-tls";
                hosts = [ domain ];
              }
            ];
          };

          monitoring.enabled = true;
          replicaCount = 1;
        };

        extraResources =
          cfg:
          optionalAttrs (cfg.admin.password != "" && cfg.reportingConnections != [ ]) {
            jobs."${name}-register-connections" = {
              metadata.annotations = {
                "argocd.argoproj.io/hook" = "Sync";
                "argocd.argoproj.io/hook-delete-policy" = "BeforeHookCreation,HookSucceeded";
                # Runs after the chart's own Deployment (wave "0", implicit) is created --
                # the job itself polls /api/health so it tolerates the pod not being Ready
                # yet, it just needs the Service to exist to resolve.
                "argocd.argoproj.io/sync-wave" = "1";
              };
              spec = {
                backoffLimit = 3;
                template.spec = {
                  restartPolicy = "OnFailure";
                  containers = [
                    {
                      name = "register-connections";
                      image = "ghcr.io/lillecarl/nix-csi/scratch:1.0.1";
                      command = [
                        "bash"
                        "-c"
                        registerConnectionsScript
                      ];
                      envFrom = [
                        { secretRef.name = admin-secret; }
                        { secretRef.name = connections-secret; }
                      ];
                      volumeMounts = [
                        {
                          name = "nix";
                          mountPath = "/nix";
                          subPath = "nix";
                        }
                      ];
                    }
                  ];
                  volumes = [
                    {
                      name = "nix";
                      csi = {
                        driver = "nix.csi.store";
                        volumeAttributes.nixExpr = toolsExpr;
                      };
                    }
                  ];
                };
              };
            };
          };
      };
}
