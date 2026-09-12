{ secrets, ... }:
{
  services.pinchflat = {
    enable = true;
    hostAffinity = "edgenix";

    ingressProvider = "traefik-lan";
    nfsTarget = "nas";
    nfsSubPath = "Pinchflat";
    nfs.enable = true;

    secretKeyBase = secrets.pinchflat.secretKeyBase;

    homepage.group = "Media";

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides = {
      config.volumeHandle = "pvc-b4c68721-cd07-48d7-8ca5-0d5bbff3298e";
    };
  };
}
