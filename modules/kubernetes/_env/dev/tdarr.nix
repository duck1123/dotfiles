{ config, ... }:
{
  services.tdarr = {
    enable = false;
    image = "ghcr.io/haveagitgat/tdarr:2.86.01";
    healthcheckcpuWorkers = 0;
    healthcheckgpuWorkers = 1;
    hostAffinity = "nasnix";

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

    replicas = 1;
    storageClassName = "longhorn";
    useProbes = false;
    vpn.enable = false;
    enableGPU = true;
    enableNvidiaGPU = false;
    transcodecpuWorkers = 0;
    transcodegpuWorkers = 0;

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides = {
      config.volumeHandle = "pvc-b11637b3-e2c3-4f93-82a3-2a19c53d0aff";
      temp.volumeHandle = "pvc-157c8a73-0af2-4625-b441-2183651d25b5";
    };
  };
}
