{ config, secrets, ... }:
{
  services.bookorbit = {
    enable = true;

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Media";

    databaseTarget = "postgresql";

    # Same NAS library booklore/komga already point at.
    nfsTarget = "nas";
    nfsSubPath = "Books";
    nfs.enable = true;
    # Second export, mounted at /books/Audiobooks -- same NAS folder
    # audiobookshelf already points at.
    nfs.audiobooksPath = "${config.devDefaults.nasBase}/Audiobooks";

    jwtSecret = (secrets.bookorbit or { }).jwtSecret or "";
    setupBootstrapToken = (secrets.bookorbit or { }).setupBootstrapToken or "";

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides = {
      data.volumeHandle = "pvc-51d3f19b-d08c-4486-8da0-586d5cf841fe";
    };
  };
}
