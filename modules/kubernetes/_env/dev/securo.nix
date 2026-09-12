{ secrets, ... }:
{
  services.securo = {
    enable = true;

    databaseTarget = "postgresql";

    # Alembic's `config.set_main_option("sqlalchemy.url", ...)` runs the URL
    # through Python's ConfigParser, which treats "%" as interpolation
    # syntax -- any percent-encoded special character in the shared
    # databaseProviders.postgresql password (e.g. `)`, `:`, `+`) crashes
    # migrations with "invalid interpolation syntax". Give securo its own
    # alphanumeric-only password so the DSN never contains a literal "%".
    database.password = secrets.securo.database.password;

    secretKey = secrets.securo.secretKey;

    redis = {
      host = "redis.redis";
      port = 6379;
      password = secrets.redis.password;

      # db 0 (the default for every app on the shared redis instance) is a
      # free-for-all: immich (BullMQ), nocodb, and any Celery-based app all
      # write there, and Celery's default result-backend keys
      # ("celery-task-meta-<uuid>") and pidbox control/reply queue have no
      # per-app prefix at all -- they collide directly between unrelated
      # Celery apps. securo-worker crash-looped on startup picking up a
      # stale application/x-signed-pickle pidbox message left behind by
      # tube-archivist's Celery setup (see
      # incident-tube-archivist-celery-pickle-crashloop in memory) even
      # though tube-archivist is disabled -- the poisoned message was still
      # sitting in db 0. Giving securo its own db avoids that collision
      # without touching any other app's (currently-working) redis config.
      dbIndex = 2;
    };

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Finance";

    storageClassName = "longhorn";

    volumeOverrides = {
      attachments.volumeHandle = "pvc-26f98910-ffc8-42d0-a147-9662c23cd2ae";
      agent-knowledge.volumeHandle = "pvc-f37b9cbf-5ffc-4e38-bffe-c8dabbceda5a";
      embedding-models.volumeHandle = "pvc-94cacd5e-4f3a-4100-a179-fad968db8019";
    };
  };
}
