{ config, ... }:
{
  services.cloudbeaver = {
    enable = true;
    hostAffinity = "edgenix";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Database";

    storageClassName = "longhorn";

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides.data.volumeHandle = "pvc-44f776b3-80c5-4edc-9398-e681cab79b16";
  };
}
