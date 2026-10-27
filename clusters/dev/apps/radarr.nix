{ config, secrets, ... }:
{
  services.radarr = {
    databaseTarget = "postgresql";
    database.enable = true;

    apiKey = secrets.radarr.key;
    hostAffinity = "edgenix";
    image = "linuxserver/radarr:6.4.4.10685-ls324";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Arr";

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}";
    };

    storageClassName = "longhorn";
    vpn.enable = false;
  };
}
