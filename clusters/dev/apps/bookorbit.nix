{ config, secrets, ... }:
{
  services.bookorbit = {
    enable = false;

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Media";

    databaseTarget = "postgresql";

    # Same NAS library komga already points at.
    nfsTarget = "nas";
    nfsSubPath = "Books";
    nfs.enable = true;
    # Second export, mounted at /books/Audiobooks -- same NAS folder
    # audiobookshelf already points at.
    nfs.audiobooksPath = "${config.devDefaults.nasBase}/Audiobooks";

    jwtSecret = (secrets.bookorbit or { }).jwtSecret or "";
    setupBootstrapToken = (secrets.bookorbit or { }).setupBootstrapToken or "";
  };
}
