{ config, ... }:
{
  services.tdarr = {
    image = "ghcr.io/haveagitgat/tdarr:2.86.01";
    healthcheckcpuWorkers = 1;
    healthcheckgpuWorkers = 1;
    # hostAffinity = "nasnix";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Automation";

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = config.devDefaults.nasBase;
    };

    puid = 1000;
    pgid = 1000;

    storageClassName = "longhorn";
    useProbes = true;
    vpn.enable = false;
    enableGPU = true;
    enableNvidiaGPU = false;
    transcodecpuWorkers = 0;
    transcodegpuWorkers = 0;
  };
}
