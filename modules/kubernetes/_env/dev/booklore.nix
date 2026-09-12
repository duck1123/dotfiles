{ ... }:
{
  services.booklore = {
    enable = false;
    # hostAffinity = "edgenix";

    databaseTarget = "mariadb";

    gid = "0";

    ingressProvider = "traefik-lan";

    nfsTarget = "nas";
    nfsSubPath = "Books";
    nfs.enable = true;

    uid = "0";

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides = {
      data.volumeHandle = "pvc-da71c5a0-68e9-48f0-a8a2-e71d5a8adccc";
      bookdrop.volumeHandle = "pvc-b8bc2a4f-b836-4142-a5cf-c513c51f5422";
    };
  };
}
