{ config, ... }:
{
  services.sabnzbd = {
    enable = true;
    hostAffinity = "edgenix";

    ingress = {
      domain = "sabnzbd.${config.devDefaults.homeDomain}";
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
    useProbes = false;
  };
}
