{ config, ... }:
{
  services.audiobookshelf = {
    enable = true;
    hostAffinity = "edgenix";

    ingress = {
      domain = "audiobookshelf.${config.devDefaults.homeDomain}";
      ingressClassName = "traefik";
      clusterIssuer = config.devDefaults.clusterIssuer;
      tls.enable = true;
    };

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}/Audiobooks";
    };

    storageClassName = "longhorn";
  };
}
