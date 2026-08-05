{ config, ... }:
{
  services.cloudbeaver = {
    enable = true;
    hostAffinity = "edgenix";

    ingress = {
      domain = "cloudbeaver.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
      clusterIssuer = "tailscale";
    };

    storageClassName = "longhorn";
  };
}
