{ config, secrets, ... }:
{
  services.lidarr = {
    enable = true;

    ingress = {
      domain = "lidarr.${config.devDefaults.homeDomain}";
      ingressClassName = "traefik";
      clusterIssuer = config.devDefaults.clusterIssuer;
      tls.enable = true;
    };

    vpn = {
      enable = false;
      sharedGluetunService = "gluetun.gluetun";
    };

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}";

      slskdDownloads = {
        enable = true;
        path = "${config.devDefaults.nasBase}/slskd_downloads";
      };
    };

    database = {
      enable = true;
      host = "postgresql.postgresql";
      port = 5432;
      name = "lidarr";
      username = "lidarr";
      password = secrets.postgresql.userPassword;
    };

    hostAffinity = "edgenix";

    replicas = 1;
    storageClassName = "longhorn";
  };
}
