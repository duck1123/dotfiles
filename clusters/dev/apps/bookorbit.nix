{ config, secrets, ... }:
{
  services.bookorbit = {
    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Media";

    databaseTarget = "postgresql";

    nfsTarget = "nas";
    nfsSubPath = "Books";
    nfs.enable = true;
    # Second export, mounted at /books/Audiobooks -- same NAS folder
    # audiobookshelf already points at.
    nfs.audiobooksPath = "${config.devDefaults.nasBase}/Audiobooks";
    # qBittorrent's downloads share, at the same /downloads path it reports,
    # so book requests can import finished downloads.
    nfs.downloadsPath = "${config.devDefaults.nasBase}/Downloads";

    jwtSecret = (secrets.bookorbit or { }).jwtSecret or "";
    setupBootstrapToken = (secrets.bookorbit or { }).setupBootstrapToken or "";
    bookRequestEncryptionKey = (secrets.bookorbit or { }).bookRequestEncryptionKey or "";
  };
}
