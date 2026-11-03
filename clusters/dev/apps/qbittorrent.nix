{ config, secrets, ... }:
{
  services.qbittorrent = {
    hostAffinity = "edgenix";

    homepage.group = "Download";

    ingressProvider = "traefik-lan";

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
