{ config, ... }:
{
  services.trilium = {
    enable = true;

    ingress = {
      domain = "trilium.${config.devDefaults.homeDomain}";
      ingressClassName = "traefik";
      clusterIssuer = config.devDefaults.clusterIssuer;
      tls.enable = true;
    };

    storageClassName = "longhorn";
  };
}
