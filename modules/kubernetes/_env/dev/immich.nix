{ config, secrets, ... }:
{
  services.immich = {
    enable = true;

    database = {
      inherit (secrets.immich.database) password username;
      host = "postgresql.postgresql";
      port = 5432;
      name = "immich";
    };

    ingress = {
      domain = "immich.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
      clusterIssuer = "tailscale";
    };

    nfs.enable = false;

    externalLibrary = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}/Photos";
    };

    redis = {
      inherit (secrets.immich.redis) password;
      host = "redis.redis";
      port = 6379;
      dbIndex = 0;
    };

    storageClassName = "longhorn";
  };
}
