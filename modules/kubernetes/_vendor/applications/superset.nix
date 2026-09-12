{ ... }:
{
  flake.nixidyApps.superset =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      name = "superset";
      env-secret = "superset-env";
      admin-secret = "superset-admin";
      connections-secret = "superset-postgres-connections";
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
        uses-ingress = true;
        uses-database = true;

        # https://artifacthub.io/packages/helm/apache-superset/superset
        chart = lib.helm.downloadHelmChart {
          repo = "https://apache.github.io/superset";
          chart = "superset";
          version = "0.22.7";
          chartHash = "sha256-modNfk4rxbEfHi/LRYnYbCXh0o1kfmZ6JTpmlJhZ5OQ=";
        };

        # The chart's own `secretEnv.create`/`extraSecretEnv`/`config.SECRET_KEY` all bake
        # literal values straight into a plain (non-sops) Secret's `stringData` at `helm
        # template` time -- since nixidy renders that once into manifests/dev/ and commits it,
        # any real secret placed there would leak in plaintext. So `secretEnv.create` is
        # disabled below and this repo supplies its own "superset-env" Secret (named to match
        # the chart's default `envFromSecret`) via sopsSecrets instead -- every container
        # (web, worker, beat, init job) already loads all its DB/Redis config and SECRET_KEY
        # from that one secret via `envFrom`, so no chart values need to carry them.
        sopsSecrets =
          cfg:
          {
            ${env-secret} = {
              DB_HOST = cfg.database.host;
              DB_PORT = toString cfg.database.port;
              DB_USER = cfg.database.username;
              DB_PASS = cfg.database.password;
              DB_NAME = cfg.database.name;
              REDIS_HOST = cfg.redis.host;
              REDIS_PORT = toString cfg.redis.port;
              REDIS_USER = "";
              REDIS_PASSWORD = cfg.redis.password;
              REDIS_DB = toString cfg.redis.cacheDbIndex;
              REDIS_CELERY_DB = toString cfg.redis.celeryDbIndex;
              REDIS_PROTO = "redis";
              SECRET_KEY = cfg.secretKey;
            };
          }
          // optionalAttrs (cfg.admin.password != "") {
            ${admin-secret} = {
              username = cfg.admin.username;
              email = cfg.admin.email;
              password = cfg.admin.password;
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

        extraOptions = {
          imageTag = mkOption {
            description = mdDoc "Superset image tag (chart AppVersion is the default upstream would use; pinned explicitly here so the superset-create-admin job's image always matches the main deployment)";
            type = types.str;
            default = "6.1.0";
          };

          redis = {
            host = mkOption {
              description = mdDoc "Redis host (shared cluster instance)";
              type = types.str;
              default = "redis.redis";
            };
            port = mkOption {
              description = mdDoc "Redis port";
              type = types.int;
              default = 6379;
            };
            password = mkOption {
              description = mdDoc "Redis password";
              type = types.str;
              default = "";
            };
            # Dedicated db indices (not 0/2, already used by other apps sharing this Redis --
            # see incident-tube-archivist-celery-pickle-crashloop in memory for why colliding
            # Celery apps on the same db is dangerous) so Superset's own Celery broker/results
            # backend never collides with another app's keys.
            cacheDbIndex = mkOption {
              description = mdDoc "Redis database index for Superset's query/metadata cache";
              type = types.int;
              default = 3;
            };
            celeryDbIndex = mkOption {
              description = mdDoc "Redis database index for Superset's Celery broker/result backend";
              type = types.int;
              default = 4;
            };
          };

          secretKey = mkOption {
            description = mdDoc ''
              Flask `SECRET_KEY` used to sign sessions/cookies (generate: `openssl rand -base64 42`).
              Stored in secrets.enc.yaml as `superset.secretKey`; changing it invalidates all
              existing sessions.
            '';
            type = types.str;
            default = "";
          };

          admin = {
            username = mkOption {
              description = mdDoc "Initial admin username, created by the superset-create-admin job";
              type = types.str;
              default = "admin";
            };
            email = mkOption {
              description = mdDoc "Initial admin email";
              type = types.str;
              default = "admin@superset.local";
            };
            password = mkOption {
              description = mdDoc ''
                Initial admin password (generate: `openssl rand -hex 20`). Stored in
                secrets.enc.yaml as `superset.admin.password`. Left empty, the
                superset-create-admin job is skipped entirely -- set it once, let the job
                run, then rotate the password from within Superset if desired.
              '';
              type = types.str;
              default = "";
            };
          };

          celeryBeat.enable = mkOption {
            description = mdDoc "Run the Celery beat scheduler -- only needed for scheduled alerts/reports";
            type = types.bool;
            default = false;
          };

          reportingConnections = mkOption {
            description = mdDoc ''
              Postgres databases to register as SQL Lab connections, one entry per
              database (Postgres has no cross-database queries, so a single connection
              can't cover multiple databases on the same instance). Registered
              declaratively by the superset-register-connections job -- see
              env/dev/superset.nix, which populates this from
              config.services.postgresql.extraDatabases so it stays in sync with that
              list automatically. Each entry should use that database's own
              least-privilege role rather than the Postgres admin role.
            '';
            type = types.listOf (
              types.submodule {
                options = {
                  name = mkOption {
                    type = types.str;
                    description = mdDoc "Database name -- also used as the Superset connection display name.";
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

        defaultValues = cfg: {
          secretEnv.create = false;

          image.tag = cfg.imageTag;

          # Superset ships its own postgres/redis (bitnami-family) subcharts; this cluster
          # already has both shared, so both stay disabled -- see uses-database above and
          # cfg.redis.* for where the connection details actually come from.
          postgresql.enabled = false;
          redis.enabled = false;

          cache = {
            enabled = true;
            cacheDb = cfg.redis.cacheDbIndex;
            celeryDb = cfg.redis.celeryDbIndex;
          };

          # The stock apachesuperset.docker.scarf.sh/apache/superset image ships no Postgres
          # DBAPI driver at all (verified: neither psycopg2 nor psycopg importable), and the
          # /app/.venv the app actually runs from has no `pip` at all -- a bare `pip install`
          # resolves to a *different* (system) Python whose packages the venv never sees
          # ("Successfully installed" but still ModuleNotFoundError at import time), and
          # /app/.venv/bin/pip doesn't exist. Confirmed working recipe (verified against this
          # exact image/tag via a one-off debug pod): `ensurepip` into the venv first, then
          # `python3 -m pip install` using that venv's own interpreter. Re-runs on every
          # container start (no persistent volume backs ~/bootstrap).
          bootstrapScript = ''
            #!/bin/bash
            if [ ! -f ~/bootstrap ]; then
              /app/.venv/bin/python3 -m ensurepip
              /app/.venv/bin/python3 -m pip install --no-cache-dir psycopg2-binary
              echo "Running Superset with uid {{ .Values.runAsUser }}" > ~/bootstrap
            fi
          '';

          # Safe to bake in literally -- just Python reading an env var, not a secret value.
          # The real SECRET_KEY lives in the "superset-env" Secret above.
          configOverrides.secret_key = ''
            SECRET_KEY = os.environ.get("SECRET_KEY")
          '';

          # RESULTS_BACKEND (SQL Lab's async query-result store, distinct from
          # CACHE_CONFIG/CELERY_CONFIG which the chart already builds correctly from the
          # REDIS_* env vars) is the one place the chart bakes `cache.password` as a literal
          # Python kwarg at `helm template` time instead of reading it from env -- leaving
          # `cache.password` unset (as above) keeps it unauthenticated and unable to reach
          # this cluster's password-protected Redis. `config.resultsBackend`, when a string,
          # is inserted verbatim as the RHS of `RESULTS_BACKEND = ...` (see the chart's
          # `superset.config` helper), so this rebuilds the same RedisCache from env instead.
          config.resultsBackend = ''RedisCache(host=env("REDIS_HOST"), port=int(env("REDIS_PORT", "6379")), password=env("REDIS_PASSWORD", ""), db=int(env("REDIS_DB", "1")), key_prefix="superset_results")'';

          # The built-in init job would otherwise bake init.adminUser.password as a literal
          # `superset fab create-admin --password ...` CLI arg into its (plaintext, chart
          # -managed) config Secret. createAdmin is disabled here and a separate
          # superset-create-admin job (extraResources below) does it instead, reading the
          # password from a secretKeyRef so it never appears in a rendered manifest.
          init.createAdmin = false;

          supersetCeleryBeat.enabled = cfg.celeryBeat.enable;

          ingress = with cfg.ingress; {
            enabled = true;
            inherit ingressClassName;
            annotations."cert-manager.io/cluster-issuer" = clusterIssuer;
            hosts = [ domain ];
            tls = [
              {
                secretName = "superset-tls";
                hosts = [ domain ];
              }
            ];
          };
        };

        extraResources = cfg: {
          jobs = optionalAttrs (cfg.admin.password != "") {
            "${name}-create-admin" = {
              metadata.annotations = {
                "argocd.argoproj.io/hook" = "Sync";
                "argocd.argoproj.io/hook-delete-policy" = "BeforeHookCreation,HookSucceeded";
                # Runs after the chart's own init-db job (implicit wave "0") has upgraded the
                # schema and initialized roles -- ArgoCD waits for wave 0 to be Healthy (the
                # init-db Job Succeeded) before starting wave 1.
                "argocd.argoproj.io/sync-wave" = "1";
              };
              spec = {
                backoffLimit = 3;
                template.spec = {
                  restartPolicy = "OnFailure";
                  securityContext.runAsUser = 0;
                  containers = [
                    {
                      name = "create-admin";
                      image = "apachesuperset.docker.scarf.sh/apache/superset:${cfg.imageTag}";
                      imagePullPolicy = "IfNotPresent";
                      envFrom = [ { secretRef.name = env-secret; } ];
                      env = [
                        {
                          name = "ADMIN_USERNAME";
                          valueFrom.secretKeyRef = {
                            name = admin-secret;
                            key = "username";
                          };
                        }
                        {
                          name = "ADMIN_EMAIL";
                          valueFrom.secretKeyRef = {
                            name = admin-secret;
                            key = "email";
                          };
                        }
                        {
                          name = "ADMIN_PASSWORD";
                          valueFrom.secretKeyRef = {
                            name = admin-secret;
                            key = "password";
                          };
                        }
                      ];
                      command = [
                        "/bin/sh"
                        "-c"
                        ''
                          . /app/pythonpath/superset_bootstrap.sh
                          superset fab create-admin \
                            --username "$ADMIN_USERNAME" \
                            --firstname Superset \
                            --lastname Admin \
                            --email "$ADMIN_EMAIL" \
                            --password "$ADMIN_PASSWORD" || true
                        ''
                      ];
                      volumeMounts = [
                        {
                          name = "superset-config";
                          mountPath = "/app/pythonpath";
                          readOnly = true;
                        }
                      ];
                    }
                  ];
                  volumes = [
                    {
                      name = "superset-config";
                      secret.secretName = "${name}-config";
                    }
                  ];
                };
              };
            };
          }
          // optionalAttrs (cfg.reportingConnections != [ ]) {
            "${name}-register-connections" = {
              metadata.annotations = {
                "argocd.argoproj.io/hook" = "Sync";
                "argocd.argoproj.io/hook-delete-policy" = "BeforeHookCreation,HookSucceeded";
                # Same wave as superset-create-admin -- both only need the chart's own
                # init-db job (wave "0") to have finished, and don't depend on each other.
                "argocd.argoproj.io/sync-wave" = "1";
              };
              spec = {
                backoffLimit = 3;
                template.spec = {
                  restartPolicy = "OnFailure";
                  securityContext.runAsUser = 0;
                  containers = [
                    {
                      name = "register-connections";
                      image = "apachesuperset.docker.scarf.sh/apache/superset:${cfg.imageTag}";
                      imagePullPolicy = "IfNotPresent";
                      envFrom = [
                        { secretRef.name = env-secret; }
                        { secretRef.name = connections-secret; }
                      ];
                      command = [
                        "/bin/sh"
                        "-c"
                        ''
                          set -e
                          . /app/pythonpath/superset_bootstrap.sh
                          python3 - <<'PYEOF'
                          import json
                          import os

                          from sqlalchemy.engine import URL

                          from superset.app import create_app

                          app = create_app()
                          with app.app_context():
                              from superset import db
                              from superset.models.core import Database

                              conns = json.loads(os.environ["CONNECTIONS_JSON"])
                              for c in conns:
                                  uri = str(
                                      URL.create(
                                          "postgresql+psycopg2",
                                          username=c["username"],
                                          password=c["password"],
                                          host=c["host"],
                                          port=c["port"],
                                          database=c["name"],
                                      )
                                  )
                                  existing = (
                                      db.session.query(Database)
                                      .filter_by(database_name=c["name"])
                                      .one_or_none()
                                  )
                                  if existing is None:
                                      existing = Database(database_name=c["name"])
                                      db.session.add(existing)
                                  existing.set_sqlalchemy_uri(uri)
                              db.session.commit()
                          PYEOF
                        ''
                      ];
                      volumeMounts = [
                        {
                          name = "superset-config";
                          mountPath = "/app/pythonpath";
                          readOnly = true;
                        }
                      ];
                    }
                  ];
                  volumes = [
                    {
                      name = "superset-config";
                      secret.secretName = "${name}-config";
                    }
                  ];
                };
              };
            };
          };
        };
      };
}
