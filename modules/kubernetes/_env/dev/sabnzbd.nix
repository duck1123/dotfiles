{ config, ... }:
{
  services.sabnzbd = {
    enable = true;
    hostAffinity = "edgenix";

    ingress = {
      domain = "sabnzbd.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
      clusterIssuer = "tailscale";
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
