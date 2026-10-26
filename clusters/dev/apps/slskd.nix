{ config, secrets, ... }:
{
  services.slskd = {
    # Pinned: with IfNotPresent, `:latest` meant whatever each node had cached
    # (edgenix still had 0.24.3, which crashes on the schema 0.26 migrated
    # to). 0.26.1 is what ran the 2026-10-20 migration and ran cleanly after.
    image = "slskd/slskd:0.26.1";
    apiKey = (secrets.slskd or { }).apiKey or "";
    # hostAffinity = "edgenix";

    homepage.group = "Download";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}/slskd_downloads";
    };

    shares = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}/Music";
    };

    storageClassName = "longhorn";
    useProbes = false;

    vpn = {
      enable = true;
      sharedGluetunService = "gluetun.gluetun";
    };

    webAuth = {
      username = (secrets.slskd or { }).username or "";
      password = (secrets.slskd or { }).password or "";
    };
  };
}
