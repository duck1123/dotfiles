{ config, ... }:
{
  services.fileflows = {
    enable = false;
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

    replicas = 1;
    storageClassName = "longhorn";
    useProbes = false;
    enableGPU = true;

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides = {
      config.volumeHandle = "pvc-238a7fe1-5d70-45c2-8a5f-1752cf171b36";
      temp.volumeHandle = "pvc-6e5f5c78-e4d4-47ff-a407-9f1cefd37ea3";
    };
  };
}
