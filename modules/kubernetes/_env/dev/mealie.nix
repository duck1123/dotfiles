{ config, ... }:
{
  services.mealie = {
    enable = true;
    hostAffinity = "edgenix";

    ingress = {
      domain = "mealie.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
      clusterIssuer = "tailscale";
    };

    storageClassName = "longhorn";
  };
}
