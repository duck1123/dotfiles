{ config, secrets, ... }:
{
  services.qbittorrent = {
    hostAffinity = "edgenix";

    homepage.group = "Download";

    ingressProvider = "traefik-lan";

    # duck's primary group on the NAS is admin (10), which the Downloads share's
    # ugacl allows. nfsd keeps the client's primary gid when its manage-gids
    # lookup fails, so this avoids transient Permission denied (Trilium MzTddak753pb).
    pgid = 10;

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}";
    };

    vpn = {
      wireguardPrivateKey = secrets.mullvad.qbittorrentWireguardKey;
      wireguardAddresses = "10.162.206.243/32";
    };

    webui = { inherit (secrets.qbittorrent) password username; };
  };
}
