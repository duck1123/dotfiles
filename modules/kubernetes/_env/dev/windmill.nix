{ config, secrets, ... }:
{
  services.windmill = {
    enable = false;
    hostAffinity = "edgenix";
    image = "ghcr.io/windmill-labs/windmill-full:latest";

    ingress = {
      domain = "windmill.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
      clusterIssuer = "tailscale";
    };

    database = {
      host = "postgresql.postgresql";
      port = 5432;
      name = "windmill";
      username = secrets.windmill.database.username;
      password = secrets.windmill.database.password;
    };

    storageClassName = "longhorn";
    replicas = 1;
  };
}
