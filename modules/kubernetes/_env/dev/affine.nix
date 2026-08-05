{ config, secrets, ... }:
{
  services.affine = {
    enable = true;
    hostAffinity = "edgenix";

    database = {
      host = "postgresql.postgresql";
      port = 5432;
      name = "affine";
      username = "affine";
      password = secrets.postgresql.userPassword;
    };

    redis = {
      host = "redis.redis";
      port = 6379;
      password = secrets.redis.password;
    };

    serverExternalUrl = "https://affine.${config.devDefaults.tailDomain}";

    ingress = {
      domain = "affine.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
      clusterIssuer = "tailscale";
    };

    storageClassName = "longhorn";
  };
}
