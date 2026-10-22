{ config, secrets, ... }:
{
  services.qbittorrent = {
    allowControlPlane = true;
    hostAffinity = "nasnix";

    homepage.group = "Download";

    ingressProvider = "traefik-lan";

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}";
    };

    webui = { inherit (secrets.qbittorrent) password username; };
  };
}
