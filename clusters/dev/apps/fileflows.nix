{ config, ... }:
{
  services.fileflows = {
    hostAffinity = "nixmini";

    ingressProvider = "traefik-lan";

    monitoring.autokuma.enable = true;

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = config.devDefaults.nasBase;
      enableVideos = true;
    };

    puid = 1000;
    pgid = 1000;

    storageClassName = "longhorn";
    useProbes = false;
    enableGPU = true;
  };
}
