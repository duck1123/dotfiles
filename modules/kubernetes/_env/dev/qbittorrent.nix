{ config, secrets, ... }:
{
  services.qbittorrent = {
    enable = false;
    hostAffinity = "nasnix";

    ingress = {
      domain = "qbittorrent.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
      clusterIssuer = "tailscale";
    };

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}";
    };

    webui = { inherit (secrets.qbittorrent) password username; };
  };
}
