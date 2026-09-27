{ config, secrets, ... }:
{
  services.tube-archivist = {
    auth = {
      inherit (secrets.tube-archivist.auth) username password;
    };

    elasticsearch.elasticPassword = secrets.tube-archivist.auth.password;
    # Confirmed-unfixable upstream bug, tested live 2026-09-08:
    # https://github.com/tubearchivist/tubearchivist/issues/1209
    # Clearing poisoned celery-task-meta-* Redis keys gets past the startup
    # crash (web UI comes up fine), but the Celery worker then dies
    # permanently on its first "mingle" handshake with
    # ContentDisallowed: application/x-signed-pickle -- so background
    # tasks (including all downloads) can never run regardless. No fix
    # possible from our side; revisit if upstream resolves #1209.
    enable = false;
    hostAffinity = "edgenix";

    ingressProvider = "traefik-lan";

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}";
    };

    redis = {
      host = "redis.redis";
      port = 6379;
      password = secrets.redis.password;
    };
    storageClassName = "longhorn";
    replicas = 1;
  };
}
