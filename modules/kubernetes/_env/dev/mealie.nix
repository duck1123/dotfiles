{ config, ... }:
{
  services.mealie = {
    enable = true;
    hostAffinity = "edgenix";

    ingress = {
      domain = "mealie.${config.devDefaults.homeDomain}";
      ingressClassName = "traefik";
      clusterIssuer = config.devDefaults.clusterIssuer;
      tls.enable = true;
    };

    storageClassName = "longhorn";
  };
}
