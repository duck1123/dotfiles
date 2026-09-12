{ config, ... }:
{
  services.kapowarr = {
    enable = false;
    hostAffinity = "edgenix";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}";

      library = {
        enable = true;
        path = "${config.devDefaults.nasBase}/Books";
      };
    };

    replicas = 1;
    storageClassName = "longhorn";
    vpn.enable = false;

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides.config.volumeHandle = "pvc-aa4bb624-7cf8-4fbc-a6c9-7e09e50f53f6";
  };
}
