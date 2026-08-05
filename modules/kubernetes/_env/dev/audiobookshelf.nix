{ config, ... }:
{
  services.audiobookshelf = {
    enable = true;
    hostAffinity = "edgenix";

    ingress = {
      domain = "audiobookshelf.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
      clusterIssuer = "tailscale";
    };

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}/Audiobooks";
    };

    storageClassName = "longhorn";
  };
}
