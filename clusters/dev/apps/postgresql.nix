{ secrets, arrDatabases, ... }:
{
  services.postgresql = {
    auth = {
      inherit (secrets.postgresql)
        adminPassword
        adminUsername
        replicationPassword
        userPassword
        ;
    };

    enable = true;
    hostAffinity = "edgenix";
    storageClassName = "longhorn";

    extraDatabases =
      arrDatabases [
        { name = "prowlarr"; }
        { name = "sonarr"; }
        { name = "radarr"; }
        { name = "lidarr"; }
        { name = "whisparr"; }
        { name = "listenarr"; }
      ]
      ++ [
        {
          name = "attic";
          username = "attic";
          password = secrets.postgresql.userPassword;
        }
        {
          name = "immich";
          username = "immich";
          password = secrets.postgresql.userPassword;
        }
        {
          name = "gitea";
          username = "postgres";
          password = secrets.postgresql.userPassword;
        }
        {
          name = "affine";
          username = "affine";
          password = secrets.postgresql.userPassword;
        }
        {
          name = "memos";
          username = "postgres";
          password = secrets.postgresql.userPassword;
        }
        {
          name = "nocodb";
          username = "nocodb";
          password = secrets.postgresql.userPassword;
        }
        {
          name = "romm";
          username = "postgres";
          password = secrets.postgresql.userPassword;
        }
        {
          name = "nostrarchives";
          username = "nostrarchives";
          password = secrets.postgresql.userPassword;
        }
        {
          name = "xyops";
          username = "xyops";
          password = secrets.postgresql.userPassword;
        }
        {
          name = "paperless-ngx";
          username = "paperless-ngx";
          password = secrets.postgresql.userPassword;
        }
        {
          name = "bookorbit";
          username = "bookorbit";
          password = secrets.postgresql.userPassword;
          # BookOrbit 2.x requires these for its migrations/search; the app's
          # own DB role can't CREATE EXTENSION for itself -- see securo above.
          extensions = [
            "vector"
            "uuid-ossp"
            "pg_trgm"
          ];
        }
        {
          # Dedicated alphanumeric-only password -- see env/dev/securo.nix
          # for why an Alembic-migrated app can't share the punctuation-heavy
          # shared postgresql userPassword.
          name = "mediamanager";
          username = "mediamanager";
          password = secrets.mediamanager.database.password;
        }
        {
          # Dedicated alphanumeric-only password -- see env/dev/securo.nix
          # for why securo can't share the punctuation-heavy postgresql
          # userPassword (Alembic's ConfigParser-based URL handling breaks
          # on percent-encoded special characters).
          name = "securo";
          username = "securo";
          password = secrets.securo.database.password;
          # Securo's agent knowledge base uses pgvector; must be created here
          # (as the postgres superuser) since even the database owner can't
          # CREATE EXTENSION for it -- see the postgresql-init-databases job.
          extensions = [ "vector" ];
        }
        {
          # Dedicated alphanumeric-only password -- Superset's own `superset db upgrade`
          # goes through Alembic/ConfigParser the same way securo's does (see
          # env/dev/securo.nix), so it can't share the punctuation-heavy shared
          # userPassword either.
          name = "superset";
          username = "superset";
          password = (secrets.superset or { }).database.password or secrets.postgresql.userPassword;
        }
      ];

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides = {
      data.volumeHandle = "pvc-f011e680-b80d-4628-abbd-8641d837938b";
      backups.volumeHandle = "pvc-4a9e7907-8262-4938-b52b-3b5971935619";
    };
  };
}
