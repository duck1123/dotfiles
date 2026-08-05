{ config, secrets, ... }:
{
  services.radarr = {
    database = {
      enable = true;
      host = "postgresql.postgresql";
      port = 5432;
      name = "radarr";
      username = "radarr";
      password = secrets.postgresql.userPassword;
    };

    enable = true;
    hostAffinity = "edgenix";
    image = "linuxserver/radarr:6.0.4.10291-ls295";

    ingress = {
      domain = "radarr.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
      clusterIssuer = "tailscale";
    };

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}";
    };

    replicas = 1;
    storageClassName = "longhorn";
    vpn.enable = false;
  };
}
