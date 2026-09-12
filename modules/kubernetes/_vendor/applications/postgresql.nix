{ ... }:
let
  name = "postgresql";
  password-secret = "${name}-password";
in
{
  flake.nixidyApps.${name} =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
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

        # `pvcName` overrides since these predate the "${name}-${name}-<key>"
        # convention (data's is flat, matching persistentVolumeClaimName
        # below). Shape only -- no volumeHandle here, that's
        # environment-specific (see env/dev/postgresql.nix and
        # docs/pinned-volumes.md). Each key only exists at all when its
        # gating option is on, same as before.
        volumes =
          cfg:
          lib.optionalAttrs cfg.persistenceEnabled {
            data = {
              pvcName = "postgresql-data";
              size = cfg.persistenceSize;
            };
          }
          // lib.optionalAttrs cfg.backup.enable {
            backups = {
              pvcName = "${name}-backups";
              size = cfg.backup.storageSize;
            };
          };

        sopsSecrets = cfg: {
          ${password-secret} = with cfg.auth; {
            inherit
              adminPassword
              adminUsername
              replicationPassword
              userPassword
              ;
          };
        };

        # https://artifacthub.io/packages/helm/bitnami/redis
        chart = lib.helm.downloadHelmChart {
          repo = "https://groundhog2k.github.io/helm-charts/";
          chart = "postgres";
          version = "1.6.8";
          chartHash = "sha256-PGibzbMWRHOUaYQ8wSW6ZG/lQrWsbT1XWg+VDQMWnAg=";
        };

        extraOptions = {
          auth = {
            adminPassword = mkOption {
              description = mdDoc "The admin password";
              type = types.str;
              default = "CHANGEME";
            };

            adminUsername = mkOption {
              description = mdDoc "The admin username";
              type = types.str;
              default = "admin";
            };

            replicationPassword = mkOption {
              description = mdDoc "The replication password";
              type = types.str;
              default = "CHANGEME";
            };

            userPassword = mkOption {
              description = mdDoc "The user password";
              type = types.str;
              default = "postgres";
            };
          };

          image = mkOption {
            description = mdDoc "The PostgreSQL image (should include pgvector for Immich)";
            type = types.str;
            default = "pgvector/pgvector:pg17";
          };

          persistenceSize = mkOption {
            description = mdDoc "Size of the persistent volume";
            type = types.str;
            default = "20Gi";
          };

          persistenceEnabled = mkOption {
            description = mdDoc "Enable persistent storage for PostgreSQL";
            type = types.bool;
            default = true;
          };

          healthCheck = {
            livenessPeriodSeconds = mkOption {
              description = mdDoc "How often to perform liveness probe (seconds)";
              type = types.int;
              default = 60;
            };

            livenessTimeoutSeconds = mkOption {
              description = mdDoc "Timeout for liveness probe (seconds)";
              type = types.int;
              default = 30;
            };

            livenessFailureThreshold = mkOption {
              description = mdDoc "Number of failures before restarting pod";
              type = types.int;
              default = 10;
            };

            livenessInitialDelaySeconds = mkOption {
              description = mdDoc "Initial delay before starting liveness probe (seconds)";
              type = types.int;
              default = 120;
            };

            readinessPeriodSeconds = mkOption {
              description = mdDoc "How often to perform readiness probe (seconds)";
              type = types.int;
              default = 30;
            };

            readinessTimeoutSeconds = mkOption {
              description = mdDoc "Timeout for readiness probe (seconds)";
              type = types.int;
              default = 15;
            };

            readinessFailureThreshold = mkOption {
              description = mdDoc "Number of failures before marking pod not ready";
              type = types.int;
              default = 5;
            };

            startupPeriodSeconds = mkOption {
              description = mdDoc "How often to perform startup probe (seconds)";
              type = types.int;
              default = 10;
            };

            startupTimeoutSeconds = mkOption {
              description = mdDoc "Timeout for startup probe (seconds)";
              type = types.int;
              default = 10;
            };

            startupFailureThreshold = mkOption {
              description = mdDoc "Number of failures before giving up on startup";
              type = types.int;
              default = 60;
            };
          };

          backup = {
            enable = mkOption {
              description = mdDoc "Enable automated database backups";
              type = types.bool;
              default = true;
            };

            schedule = mkOption {
              description = mdDoc "Cron schedule for backups (default: daily at 2 AM)";
              type = types.str;
              default = "0 2 * * *";
            };

            retentionDays = mkOption {
              description = mdDoc "Number of days to retain backups";
              type = types.int;
              default = 30;
            };

            storageSize = mkOption {
              description = mdDoc "Storage size for backup PVC";
              type = types.str;
              default = "50Gi";
            };
          };

          extraDatabases = mkOption {
            description = mdDoc "Additional databases to create (list of {name, username, password})";
            type = types.listOf (
              types.submodule {
                options = {
                  name = mkOption {
                    type = types.str;
                    description = mdDoc "Database name";
                  };
                  username = mkOption {
                    type = types.str;
                    description = mdDoc "Database username";
                  };
                  password = mkOption {
                    type = types.str;
                    description = mdDoc "Database password";
                  };
                  extensions = mkOption {
                    type = types.listOf types.str;
                    description = mdDoc "Extensions to enable in this database (e.g. [ \"vector\" ]). Created as the postgres superuser -- most extensions (pgvector included) refuse non-superusers even when they own the database, so this can't be done by the app's own role.";
                    default = [ ];
                  };
                };
              }
            );
            default = [ ];
          };
        };

        defaultValues =
          cfg:
          let
            imageStr = cfg.image;
            parts = lib.splitString "/" imageStr;
            hasExplicitRegistry =
              lib.length parts > 2 || (lib.length parts == 2 && lib.hasInfix "." (lib.head parts));
            registry = if hasExplicitRegistry then lib.head parts else "docker.io";
            repoAndTag =
              if hasExplicitRegistry then
                lib.concatStringsSep "/" (lib.tail parts)
              else
                lib.concatStringsSep "/" parts;
            tagParts = lib.splitString ":" repoAndTag;
            repository = lib.head tagParts;
            tag = if lib.length tagParts > 1 then lib.last tagParts else "latest";
          in
          {
            image = { inherit registry repository tag; };
            nodeSelector."kubernetes.io/hostname" = cfg.hostAffinity;

            persistence = {
              enabled = cfg.persistenceEnabled;
              size = cfg.persistenceSize;
              storageClass = cfg.storageClassName;
            };

            settings = {
              existingSecret = password-secret;
              superuserPassword.secretKey = "adminPassword";
            };

            storage = {
              className = cfg.storageClassName;
              size = cfg.persistenceSize;
              # Bypasses the chart's own volumeClaimTemplates (which create a
              # StatefulSet-owned, per-ordinal PVC) in favor of a plain,
              # statically-pinned PVC -- see modules/lib/mkPinnedVolume.nix.
              # persistentVolumeClaimName is checked before requestedSize, so
              # this alone is enough to skip volumeClaimTemplates regardless
              # of storage.size/requestedSize.
              persistentVolumeClaimName = "postgresql-data";
            };
          };

        extraResources = cfg: {
          # Create a Job to initialize extra databases after PostgreSQL is ready
          jobs = lib.optionalAttrs (cfg.extraDatabases != [ ]) {
            "${name}-init-databases" = {
              metadata = {
                name = "${name}-init-databases";
                namespace = cfg.namespace;
                annotations = {
                  "argocd.argoproj.io/hook" = "PostSync";
                  "argocd.argoproj.io/hook-delete-policy" = "HookSucceeded";
                };
              };
              spec = {
                template = {
                  spec = {
                    restartPolicy = "OnFailure";
                    containers = [
                      {
                        name = "init-databases";
                        image = cfg.image;
                        imagePullPolicy = "IfNotPresent";
                        env = [
                          {
                            name = "PGHOST";
                            value = "${name}.${cfg.namespace}";
                          }
                          {
                            name = "PGPORT";
                            value = "5432";
                          }
                          {
                            name = "PGUSER";
                            value = "postgres";
                          }
                          {
                            name = "PGPASSWORD";
                            valueFrom = {
                              secretKeyRef = {
                                name = password-secret;
                                key = "adminPassword";
                              };
                            };
                          }
                        ];
                        command = [
                          "sh"
                          "-c"
                          ''
                            set -e
                            ${lib.concatMapStringsSep "\n" (db: ''
                              echo "Creating database ${db.name} and user ${db.username}..."
                              psql -v ON_ERROR_STOP=1 <<-EOSQL
                                -- Create database if it doesn't exist
                                SELECT format('CREATE DATABASE %I', '${db.name}') WHERE NOT EXISTS (SELECT FROM pg_database WHERE datname = '${db.name}')\gexec

                                -- Create user if it doesn't exist, then always update password
                                DO \$\$
                                BEGIN
                                  IF NOT EXISTS (SELECT FROM pg_user WHERE usename = '${db.username}') THEN
                                    EXECUTE format('CREATE USER %I WITH PASSWORD %L', '${db.username}', '${db.password}');
                                  ELSE
                                    EXECUTE format('ALTER USER %I WITH PASSWORD %L', '${db.username}', '${db.password}');
                                  END IF;
                                END
                                \$\$;

                                -- Grant privileges (quote database name)
                                DO \$\$
                                BEGIN
                                  EXECUTE format('GRANT ALL PRIVILEGES ON DATABASE %I TO %I', '${db.name}', '${db.username}');
                                  EXECUTE format('ALTER DATABASE %I OWNER TO %I', '${db.name}', '${db.username}');
                                END
                                \$\$;
                              EOSQL
                              echo "Database ${db.name} created successfully"
                            '') cfg.extraDatabases}
                            ${lib.concatMapStringsSep "\n" (
                              db:
                              lib.optionalString (db.extensions != [ ]) ''
                                echo "Enabling extensions on ${db.name}: ${lib.concatStringsSep ", " db.extensions}..."
                                psql -v ON_ERROR_STOP=1 -d '${db.name}' <<-EOSQL
                                  ${lib.concatMapStringsSep "\n                                  " (
                                    ext: "CREATE EXTENSION IF NOT EXISTS \"${ext}\";"
                                  ) db.extensions}
                                EOSQL
                              ''
                            ) cfg.extraDatabases}
                          ''
                        ];
                      }
                    ];
                  };
                };
              };
            };
          };

          # CronJob for automated backups
          cronJobs = lib.optionalAttrs cfg.backup.enable {
            "${name}-backup" = {
              metadata = {
                name = "${name}-backup";
                namespace = cfg.namespace;
                labels = {
                  "app.kubernetes.io/name" = name;
                  "app.kubernetes.io/component" = "backup";
                };
              };
              spec = {
                schedule = cfg.backup.schedule;
                successfulJobsHistoryLimit = 3;
                failedJobsHistoryLimit = 3;
                jobTemplate = {
                  spec = {
                    template = {
                      spec = {
                        restartPolicy = "OnFailure";
                        containers = [
                          {
                            name = "backup";
                            image = cfg.image;
                            command = [
                              "sh"
                              "-c"
                              ''
                                set -e
                                BACKUP_DIR="/backups"
                                TIMESTAMP=$(date +%Y%m%d_%H%M%S)
                                BACKUP_FILE="$BACKUP_DIR/postgresql-backup-$TIMESTAMP.sql.gz"

                                echo "Starting backup at $(date)"

                                # Create backup of all databases
                                pg_dumpall \
                                  -h ${name}.${cfg.namespace} \
                                  -U postgres \
                                  -c \
                                  | gzip > "$BACKUP_FILE"

                                echo "Backup completed: $BACKUP_FILE"
                                echo "Backup size: $(du -h "$BACKUP_FILE" | cut -f1)"

                                # Clean up old backups (keep last ${toString cfg.backup.retentionDays} days)
                                find "$BACKUP_DIR" -name "postgresql-backup-*.sql.gz" -type f -mtime +${toString cfg.backup.retentionDays} -delete

                                echo "Cleanup completed. Remaining backups:"
                                ls -lh "$BACKUP_DIR"/*.sql.gz 2>/dev/null || echo "No backups found"

                                echo "Backup job completed at $(date)"
                              ''
                            ];
                            env = [
                              {
                                name = "PGPASSWORD";
                                valueFrom = {
                                  secretKeyRef = {
                                    name = password-secret;
                                    key = "adminPassword";
                                  };
                                };
                              }
                            ];
                            volumeMounts = [
                              {
                                mountPath = "/backups";
                                name = "backup-storage";
                              }
                            ];
                          }
                        ];
                        volumes = [
                          {
                            name = "backup-storage";
                            persistentVolumeClaim.claimName = cfg.volumes.backups.pvcName;
                          }
                        ];
                      };
                    };
                  };
                };
              };
            };
          };

          # Patch StatefulSet health checks and the pod's scratch/config
          # volumes (persistence itself now comes from
          # storage.persistentVolumeClaimName in defaultValues + the pinned
          # PVC above, not volumeClaimTemplates)
          statefulSets = lib.optionalAttrs cfg.persistenceEnabled {
            ${name} = {
              spec = {
                # Override volumes list and health checks
                # Merge health check overrides with Helm-generated container by name
                # Use mkForce on probe fields to override Helm chart defaults
                template = {
                  spec = {
                    containers = [
                      {
                        name = "postgres";
                        # Container runtimes default /dev/shm to ~64Mi, too
                        # small for Postgres parallel-query workers' DSM
                        # segments -- bookorbit's migrations hit this
                        # ("could not resize shared memory segment ...
                        # No space left on device") until this was added.
                        volumeMounts = [
                          {
                            mountPath = "/dev/shm";
                            name = "dshm";
                          }
                        ];
                        # Health checks with less aggressive settings (using mkForce to override Helm values)
                        livenessProbe = lib.mkForce {
                          exec = {
                            command = [
                              "sh"
                              "-c"
                              "pg_isready -h localhost"
                            ];
                          };
                          initialDelaySeconds = cfg.healthCheck.livenessInitialDelaySeconds;
                          periodSeconds = cfg.healthCheck.livenessPeriodSeconds;
                          timeoutSeconds = cfg.healthCheck.livenessTimeoutSeconds;
                          successThreshold = 1;
                          failureThreshold = cfg.healthCheck.livenessFailureThreshold;
                        };
                        readinessProbe = lib.mkForce {
                          exec = {
                            command = [
                              "sh"
                              "-c"
                              "pg_isready -h localhost"
                            ];
                          };
                          initialDelaySeconds = 10;
                          periodSeconds = cfg.healthCheck.readinessPeriodSeconds;
                          timeoutSeconds = cfg.healthCheck.readinessTimeoutSeconds;
                          successThreshold = 1;
                          failureThreshold = cfg.healthCheck.readinessFailureThreshold;
                        };
                        startupProbe = lib.mkForce {
                          exec = {
                            command = [
                              "sh"
                              "-c"
                              "pg_isready -h localhost"
                            ];
                          };
                          initialDelaySeconds = 10;
                          periodSeconds = cfg.healthCheck.startupPeriodSeconds;
                          timeoutSeconds = cfg.healthCheck.startupTimeoutSeconds;
                          successThreshold = 1;
                          failureThreshold = cfg.healthCheck.startupFailureThreshold;
                        };
                      }
                    ];
                    volumes = [
                      {
                        name = "run";
                        emptyDir = { };
                      }
                      {
                        name = "tmp";
                        emptyDir = { };
                      }
                      {
                        name = "scripts";
                        emptyDir = { };
                      }
                      {
                        name = "configs";
                        emptyDir = { };
                      }
                      {
                        name = "initscripts";
                        configMap = {
                          name = "${name}-scripts";
                          defaultMode = 365;
                        };
                      }
                      {
                        name = "dshm";
                        emptyDir = {
                          medium = "Memory";
                          sizeLimit = "1Gi";
                        };
                      }
                    ];
                  };
                };
              };
            };
          };
        };
      };
}
