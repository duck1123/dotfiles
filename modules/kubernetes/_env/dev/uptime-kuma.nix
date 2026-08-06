{ config, ... }:
{
  services.uptime-kuma = {
    enable = true;
    storageClassName = "longhorn";

    ingress = {
      domain = "uptime-kuma.${config.devDefaults.homeDomain}";
      ingressClassName = "traefik";
      clusterIssuer = config.devDefaults.clusterIssuer;
      tls.enable = true;
    };
  };
}
