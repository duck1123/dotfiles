{ ... }:
{
  services.navidrome = {
    enable = false;

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    monitoring.autokuma.enable = true;

    nfsTarget = "nas";
    nfsSubPath = "Music";
    nfs.enable = true;

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides.data.volumeHandle = "pvc-883bbf8c-bcb0-48e6-a6d3-b68277d13af8";
  };
}
