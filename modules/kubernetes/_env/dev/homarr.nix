{ config, secrets, ... }:
{
  services.homarr = {
    enable = false;
    # hostAffinity = "edgenix";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;

    monitoring.autokuma.enable = true;
    secretEncryptionKey = secrets.homarr.secretEncryptionKey;
    storageClassName = "longhorn";

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides.appdata.volumeHandle = "pvc-fde5f73c-9f19-4e30-8874-46cbe0a2f6a3";
  };
}
