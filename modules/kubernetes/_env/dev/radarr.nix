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
    image = "linuxserver/radarr:6.1.1.10360-ls304";

    ingress = {
      domain = "radarr.${config.devDefaults.homeDomain}";
      ingressClassName = "traefik";
      clusterIssuer = config.devDefaults.clusterIssuer;
      tls.enable = true;
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
