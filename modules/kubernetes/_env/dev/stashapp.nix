{ config, ... }:
{
  services.stashapp = {
    enable = true;

    ingress = {
      domain = "stashapp.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
      clusterIssuer = "tailscale";
    };

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}/Videos";
    };

    replicas = 1;
    enableGPU = true;
  };
}
