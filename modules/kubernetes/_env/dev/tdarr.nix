{ config, ... }:
{
  services.tdarr = {
    enable = true;
    healthcheckcpuWorkers = 0;
    healthcheckgpuWorkers = 1;
    # hostAffinity = "edgenix";
    hostAffinity = "powerspecnix";

    ingress = {
      domain = "tdarr.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
      clusterIssuer = "tailscale";
    };

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}";
    };

    puid = 1000;
    pgid = 1000;

    replicas = 1;
    storageClassName = "longhorn";
    useProbes = false;
    vpn.enable = false;
    enableGPU = true;
    enableNvidiaGPU = false;
    # Edgenix has two cards; WX 3200 (VAAPI) is renderD129. Mount it as renderD128 so Tdarr's hardcoded path works.
    # vaapiRenderDevice = "renderD129";
    libvaDriverName = "radeonsi";
    transcodegpuWorkers = 1;
  };
}
