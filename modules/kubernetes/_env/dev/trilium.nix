{ secrets, ... }:
{
  services.trilium = {
    enable = true;
    apiKey = secrets.trilium.key;

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Notes";

    storageClassName = "longhorn";

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides.data.volumeHandle = "pvc-582855f1-f480-4983-bf80-eb22b9f2ac0a";
  };
}
