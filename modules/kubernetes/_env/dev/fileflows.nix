{ config, ... }:
{
  services.fileflows = {
    enable = true;
    hostAffinity = "nixmini";

    ingress = {
      domain = "fileflows.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
      clusterIssuer = "tailscale";
    };

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = config.devDefaults.nasBase;
      enableVideos = true;
    };

    puid = 1000;
    pgid = 1000;

    replicas = 1;
    storageClassName = "longhorn";
    useProbes = false;
    enableGPU = true;
  };
}
