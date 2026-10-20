{ config, lib, ... }:
with lib;
let
  name = "postgresql";
  password-secret = "${name}-password";
  extra-db-secret = "${name}-extradb-passwords";

  # Databases registered (config.databases) on any provider this app serves.
  provisioned = filter (
    db:
    db.provider != null && db.provision && (config.databaseProviders.${db.provider}.app or null) == name
  ) (attrValues config.databases);
  # A database whose role is the provider's superuser is created, but the
  # role itself is left alone (no CREATE/ALTER USER, no ownership change).
  ownsRole = db: db.username != config.databaseProviders.${db.provider}.superuser;
  withRole = filter ownsRole provisioned;
  # Left out of the nightly backup: backup.exclude, plus registered databases
  # on this provider with backup = false (provisioned or not).
  backupSkip =
    cfg:
    unique (
      cfg.backup.exclude
      ++ map (db: db.name) (
        filter (
          db:
          !db.backup && db.provider != null && (config.databaseProviders.${db.provider}.app or null) == name
        ) (attrValues config.databases)
      )
    );
  # e.g. "paperless-ngx" -> "EXTRADB_PAPERLESS_NGX_PASSWORD"
  extraDbEnvVar =
    dbName: "EXTRADB_${toUpper (replaceStrings [ "-" "." ] [ "_" "_" ] dbName)}_PASSWORD";
in
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

  sopsSecrets =
    cfg:
    {
      ${password-secret} = with cfg.auth; {
        inherit
          adminPassword
          adminUsername
          replicationPassword
          userPassword
          ;
      };
    }
    // optionalAttrs (withRole != [ ]) {
      # One key per database, read by the init-databases job as env vars so
      # no password ends up in the (plaintext) Job manifest.
      ${extra-db-secret} = listToAttrs (
        map (db: {
          name = "${db.name}-password";
          value = db.password;
        }) withRole
      );
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

      keep = mkOption {
        description = mdDoc ''
          Most backup runs to keep, whatever their age. The PVC needs room for
          one more run than this while a backup is being written.
        '';
        type = types.ints.positive;
        default = 7;
      };

      exclude = mkOption {
        description = mdDoc ''
          Databases to leave out of the backup, on top of registered ones with
          `backup = false` (see config.databases). For databases nothing
          registers.
        '';
        type = types.listOf types.str;
        default = [ ];
      };

      storageSize = mkOption {
        description = mdDoc "Storage size for backup PVC";
        type = types.str;
        default = "50Gi";
      };
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
    # Creates every database registered on this provider (config.databases;
    # apps register theirs via uses-database + databaseTarget).
    jobs = lib.optionalAttrs (provisioned != [ ]) {
      "${name}-init-databases" = {
        metadata = {
          inherit (cfg) namespace;
          name = "${name}-init-databases";
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
                  inherit (cfg) image;
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
                  ]
                  ++ map (db: {
                    name = extraDbEnvVar db.name;
                    valueFrom.secretKeyRef = {
                      name = extra-db-secret;
                      key = "${db.name}-password";
                    };
                  }) withRole;
                  command = [
                    "sh"
                    "-c"
                    ''
                      set -e
                      ${lib.concatMapStringsSep "\n" (db: ''
                        echo "Creating database ${db.name}..."
                        psql -v ON_ERROR_STOP=1 -v db='${db.name}' <<'EOSQL'
                        SELECT format('CREATE DATABASE %I', :'db') WHERE NOT EXISTS (SELECT FROM pg_database WHERE datname = :'db')\gexec
                        EOSQL
                        ${lib.optionalString (ownsRole db) ''
                          echo "Setting up role ${db.username} as owner of ${db.name}..."
                          psql -v ON_ERROR_STOP=1 -v db='${db.name}' -v role='${db.username}' -v pw="''$${extraDbEnvVar db.name}" <<'EOSQL'
                          SELECT format('CREATE USER %I', :'role') WHERE NOT EXISTS (SELECT FROM pg_roles WHERE rolname = :'role')\gexec
                          SELECT format('ALTER USER %I WITH PASSWORD %L', :'role', :'pw')\gexec
                          SELECT format('GRANT ALL PRIVILEGES ON DATABASE %I TO %I', :'db', :'role')\gexec
                          SELECT format('ALTER DATABASE %I OWNER TO %I', :'db', :'role')\gexec
                          EOSQL
                        ''}
                        echo "Database ${db.name} ready"
                      '') provisioned}
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
                      ) provisioned}
                      ${lib.concatMapStringsSep "\n" (
                        db:
                        lib.optionalString db.readAllTables ''
                          echo "Granting ${db.username} read access to all tables in ${db.name}..."
                          psql -v ON_ERROR_STOP=1 -d '${db.name}' <<-EOSQL
                            GRANT USAGE ON SCHEMA public TO "${db.username}";
                            GRANT SELECT ON ALL TABLES IN SCHEMA public TO "${db.username}";
                            ALTER DEFAULT PRIVILEGES FOR ROLE postgres IN SCHEMA public GRANT SELECT ON TABLES TO "${db.username}";
                          EOSQL
                        ''
                      ) provisioned}
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
          inherit (cfg) namespace;
          labels = {
            "app.kubernetes.io/name" = name;
            "app.kubernetes.io/component" = "backup";
          };
        };
        spec = {
          schedule = cfg.backup.schedule;
          concurrencyPolicy = "Forbid";
          successfulJobsHistoryLimit = 3;
          failedJobsHistoryLimit = 3;
          jobTemplate = {
            spec = {
              backoffLimit = 1;
              template = {
                spec = {
                  # Never, so a failed run's pod (and its logs) stays around
                  # under failedJobsHistoryLimit.
                  restartPolicy = "Never";
                  containers = [
                    {
                      name = "backup";
                      inherit (cfg) image;
                      command = [
                        "/bin/bash"
                        "-c"
                        ''
                          # No set -e: one database failing shouldn't stop the
                          # others. pipefail makes a pg_dump failure fail its
                          # `| gzip`.
                          set -uo pipefail
                          BACKUP_DIR="/backups"
                          TIMESTAMP=$(date +%Y%m%d_%H%M%S)
                          KEEP=${toString cfg.backup.keep}
                          SKIP=" ${concatStringsSep " " (backupSkip cfg)} "
                          export PGHOST=${name}.${cfg.namespace} PGUSER=postgres
                          failed=0

                          # Files left by an interrupted run, empty dumps,
                          # anything past retentionDays, then all but the
                          # newest KEEP runs. A run is every file sharing one
                          # timestamp; the old single-file
                          # postgresql-backup-<ts>.sql.gz dumps count as runs.
                          prune() {
                            find "$BACKUP_DIR" -maxdepth 1 -type f \( -name '*.partial' -o -name '*.sql.gz' -size 0 \) -delete
                            find "$BACKUP_DIR" -maxdepth 1 -type f -name '*.sql.gz' -mtime +${toString cfg.backup.retentionDays} -delete
                            ls -1 "$BACKUP_DIR" \
                              | sed -n 's/.*[_-]\([0-9]\{8\}_[0-9]\{6\}\)\.sql\.gz$/\1/p' \
                              | sort -u | head -n -"$KEEP" \
                              | while read -r ts; do
                                  echo "Removing run $ts"
                                  rm -f "$BACKUP_DIR"/*[_-]"$ts".sql.gz
                                done
                          }

                          # dump <name> <command...>: <name>_<ts>.sql.gz, the
                          # filename `nur postgres restore` expects. Written
                          # to .partial first so a failed dump never looks
                          # like a backup.
                          dump() {
                            local out="$BACKUP_DIR/$1_$TIMESTAMP.sql.gz"
                            shift
                            if "$@" | gzip > "$out.partial" && mv "$out.partial" "$out"; then
                              echo "  ok   $(du -h "$out" | cut -f1)  $out"
                            else
                              echo "  FAIL $out"
                              rm -f "$out.partial"
                              failed=1
                            fi
                          }
                          trap 'rm -f "$BACKUP_DIR"/*.partial' EXIT

                          echo "Starting backup at $(date)"
                          # Before dumping, so a full disk can't block its
                          # own cleanup.
                          prune
                          df -h "$BACKUP_DIR"

                          dbs=$(psql -d postgres -Atc "SELECT datname FROM pg_database WHERE NOT datistemplate ORDER BY 1") \
                            || { echo "Can't list databases"; exit 1; }

                          dump globals pg_dumpall --globals-only
                          for db in $dbs; do
                            case "$SKIP" in
                              *" $db "*) echo "  skip $db" ;;
                              *) dump "$db" pg_dump --clean --if-exists --create "$db" ;;
                            esac
                          done

                          prune
                          echo "Remaining backups:"
                          ls -lh "$BACKUP_DIR"
                          df -h "$BACKUP_DIR"
                          echo "Backup job finished at $(date) (failed=$failed)"
                          exit "$failed"
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
}
