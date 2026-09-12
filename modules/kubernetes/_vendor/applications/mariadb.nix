{ ... }:
{
  flake.nixidyApps.mariadb =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      password-secret = "mariadb-password";
      extra-db-secret = "mariadb-extradb-passwords";
      # Deterministic per-database env var name the init .sh scripts read
      # their password from -- e.g. "booklore" -> "EXTRADB_BOOKLORE_PASSWORD".
      extraDbEnvVar = dbName: "EXTRADB_${lib.toUpper dbName}_PASSWORD";
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
        name = "mariadb";

        # `pvcName` overrides since these are flat names, not the usual
        # "${name}-${name}-<key>" convention -- data's matches the chart's
        # storage.persistentVolumeClaimName value below. Shape only -- no
        # volumeHandle here, that's environment-specific (see
        # env/dev/mariadb.nix and docs/pinned-volumes.md). backups only
        # exists at all when cfg.backup.enable, same as before.
        volumes =
          cfg:
          {
            data = {
              pvcName = "mariadb-data";
              size = "8Gi";
            };
          }
          // lib.optionalAttrs cfg.backup.enable {
            backups = {
              pvcName = "mariadb-backups";
              size = cfg.backup.storageSize;
            };
          };

        sopsSecrets = cfg: {
          ${password-secret} = {
            "root-password" = cfg.auth.rootPassword;
            "user-password" = cfg.auth.password;
            username = cfg.auth.username;
            database = cfg.auth.database;
          };
        }
        // lib.optionalAttrs (cfg.extraDatabases != [ ]) {
          # One key per extra database, e.g. "booklore-password" -- kept out
          # of the customScripts ConfigMap (which is plaintext, committed to
          # git) and injected into the mariadb container as env vars instead
          # (see defaultValues.env below and extraDbEnvVar).
          ${extra-db-secret} = lib.listToAttrs (
            map (db: {
              name = "${db.name}-password";
              value = db.password;
            }) cfg.extraDatabases
          );
        };

        # https://github.com/groundhog2k/helm-charts (chart "mariadb") -- bitnami/mariadb
        # was frozen behind Bitnami's Aug 2025 paid-tier restructuring (both the chart and
        # its free-tier `bitnami/mariadb:latest` image stopped getting updates). This is a
        # plain stock-image chart from the same maintainer as this repo's postgresql.nix.
        chart = lib.helm.downloadHelmChart {
          repo = "https://groundhog2k.github.io/helm-charts/";
          chart = "mariadb";
          version = "4.44";
          chartHash = "sha256-b7yqBM02dY2my3pfyCWjekv0I82nMlSOQ3ftnXU5VvY=";
        };

        extraOptions = {
          auth = {
            rootPassword = mkOption {
              description = mdDoc "The root password";
              type = types.str;
              default = "CHANGEME";
            };

            username = mkOption {
              description = mdDoc "The username";
              type = types.str;
              default = "mariadb";
            };

            password = mkOption {
              description = mdDoc "The user password";
              type = types.str;
              default = "CHANGEME";
            };

            database = mkOption {
              description = mdDoc "The database name";
              type = types.str;
              default = "mydb";
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
                };
              }
            );
            default = [ ];
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
        };

        defaultValues = cfg: {
          # Pinned to the version validated during the bitnami->groundhog2k
          # migration (test-restore + live cutover) -- see IMAGE-VERSIONS.md.
          image.tag = "11.8.9";

          settings = {
            existingSecret = password-secret;
            rootPassword.secretKey = "root-password";
          };

          # The chart's single built-in "main" database/user (mirrors what
          # auth.database/username used to configure via bitnami's `auth.*`)
          userDatabase = {
            existingSecret = password-secret;
            name.secretKey = "database";
            user.secretKey = "username";
            password.secretKey = "user-password";
          };

          # One password env var per extra database, sourced from the
          # extra-db-secret SopsSecret above -- never the plaintext value
          # itself (that only exists in cfg.extraDatabases at Nix-eval time
          # and in the sops-encrypted ciphertext committed to git).
          env = map (db: {
            name = extraDbEnvVar db.name;
            valueFrom.secretKeyRef = {
              name = extra-db-secret;
              key = "${db.name}-password";
            };
          }) cfg.extraDatabases;

          # Extra databases beyond the main one. A plain .sql file can't do
          # variable substitution, so this is a .sh script instead -- the
          # stock mariadb entrypoint sources it the same way, and it can read
          # its password from the env var above rather than having it baked
          # into this (plaintext, git-committed) ConfigMap. Backticks are
          # escaped since this heredoc is unquoted (needed for the password
          # variable to expand) -- otherwise bash would treat them as
          # command substitution.
          customScripts = lib.listToAttrs (
            map (db: {
              name = "init-${db.name}.sh";
              value = ''
                #!/bin/bash
                set -e
                mariadb -u root -p"$MARIADB_ROOT_PASSWORD" <<-EOSQL
                  CREATE DATABASE IF NOT EXISTS \`${db.name}\`;
                  CREATE USER IF NOT EXISTS '${db.username}'@'%' IDENTIFIED BY '${"$" + extraDbEnvVar db.name}';
                  GRANT ALL PRIVILEGES ON \`${db.name}\`.* TO '${db.username}'@'%';
                  FLUSH PRIVILEGES;
                EOSQL
              '';
            }) cfg.extraDatabases
          );

          nodeSelector."kubernetes.io/hostname" = cfg.hostAffinity;

          storage.persistentVolumeClaimName = cfg.volumes.data.pvcName;
        };

        extraResources = cfg: {
          # CronJob for automated backups
          cronJobs = lib.optionalAttrs cfg.backup.enable {
            "mariadb-backup" = {
              metadata = {
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
                            image = "docker.io/mariadb:11.8.9";
                            # The backups PVC's root dir is owned by root:root
                            # 0755 (standard ext4 mkfs default) -- without
                            # this, the container falls back to the image's
                            # default non-root UID and mariadb-dump fails
                            # with "Permission denied" writing into /backups.
                            securityContext.runAsUser = 0;
                            command = [
                              "/bin/bash"
                              "-c"
                              ''
                                # pipefail matters here: without it, a mariadb-dump
                                # that dies mid-stream still lets `| gzip` exit 0
                                # (it just sees its input close early), so `set -e`
                                # alone never catches a truncated backup.
                                set -eo pipefail
                                BACKUP_DIR="/backups"
                                TIMESTAMP=$(date +%Y%m%d_%H%M%S)
                                BACKUP_FILE="$BACKUP_DIR/mariadb-backup-$TIMESTAMP.sql.gz"

                                echo "Starting backup at $(date)"

                                # Create backup
                                mariadb-dump \
                                  -h mariadb.mariadb \
                                  -u root \
                                  -p"$MARIADB_ROOT_PASSWORD" \
                                  --all-databases \
                                  --single-transaction \
                                  --quick \
                                  --lock-tables=false \
                                  | gzip > "$BACKUP_FILE"

                                echo "Backup completed: $BACKUP_FILE"
                                echo "Backup size: $(du -h "$BACKUP_FILE" | cut -f1)"

                                # Clean up old backups (keep last ${toString cfg.backup.retentionDays} days)
                                find "$BACKUP_DIR" -name "mariadb-backup-*.sql.gz" -type f -mtime +${toString cfg.backup.retentionDays} -delete

                                echo "Cleanup completed. Remaining backups:"
                                ls -lh "$BACKUP_DIR"/*.sql.gz 2>/dev/null || echo "No backups found"

                                echo "Backup job completed at $(date)"
                              ''
                            ];
                            env = [
                              {
                                name = "MARIADB_ROOT_PASSWORD";
                                valueFrom = {
                                  secretKeyRef = {
                                    name = password-secret;
                                    key = "root-password";
                                  };
                                };
                              }
                            ];
                            volumeMounts = [
                              {
                                name = "backup-storage";
                                mountPath = "/backups";
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

          # ConfigMap with restore script
          configMaps = lib.optionalAttrs cfg.backup.enable {
            "mariadb-restore-script" = {
              data = {
                "restore.sh" = ''
                  #!/bin/bash
                  # MariaDB Restore Script
                  # Usage: restore.sh <backup-file.sql.gz>

                  set -e

                  if [ -z "$1" ]; then
                    echo "Usage: $0 <backup-file.sql.gz>"
                    echo "Available backups:"
                    ls -lh /backups/*.sql.gz 2>/dev/null || echo "No backups found"
                    exit 1
                  fi

                  BACKUP_FILE="$1"

                  if [ ! -f "$BACKUP_FILE" ]; then
                    echo "Error: Backup file not found: $BACKUP_FILE"
                    exit 1
                  fi

                  echo "Starting restore from: $BACKUP_FILE"
                  echo "This will replace all existing databases!"
                  read -p "Are you sure? (yes/no): " confirm

                  if [ "$confirm" != "yes" ]; then
                    echo "Restore cancelled"
                    exit 0
                  fi

                  echo "Restoring database..."
                  gunzip -c "$BACKUP_FILE" | mariadb \
                    -h mariadb.mariadb \
                    -u root \
                    -p"$MARIADB_ROOT_PASSWORD"

                  echo "Restore completed successfully!"
                '';
              };
            };
          };
        };
      };
}
