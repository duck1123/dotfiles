{ config, ... }:
{
  services.komga = {
    enable = true;

    ingress = {
      domain = "komga.${config.devDefaults.homeDomain}";
      clusterIssuer = config.devDefaults.clusterIssuer;
      ingressClassName = "traefik";
      localIngress.enable = false;
      tls.enable = true;
    };

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}/Books";
    };
  };
}
