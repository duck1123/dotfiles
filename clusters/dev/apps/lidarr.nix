{ config, secrets, ... }:
{
  services.lidarr = {
    databaseTarget = "postgresql";
    database.enable = true;

    enable = true;
    apiKey = secrets.lidarr.key;
    hostAffinity = "edgenix";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Arr";

    monitoring.autokuma.enable = true;

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}";

      slskdDownloads = {
        enable = true;
        path = "${config.devDefaults.nasBase}/slskd_downloads";
      };
    };

    replicas = 1;
    storageClassName = "longhorn";

    vpn = {
      enable = false;
      sharedGluetunService = "gluetun.gluetun";
    };

  };
}
