{ config, secrets, ... }:
{
  services.slskd = {
    enable = true;

    ingress = {
      domain = "slskd.${config.devDefaults.homeDomain}";
      ingressClassName = "traefik";
      clusterIssuer = config.devDefaults.clusterIssuer;
      tls.enable = true;
    };

    hostAffinity = "edgenix";

    webAuth = {
      username = (secrets.slskd or { }).username or "";
      password = (secrets.slskd or { }).password or "";
    };

    apiKey = (secrets.slskd or { }).apiKey or "";

    vpn = {
      enable = true;
      sharedGluetunService = "gluetun.gluetun";
    };

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

    replicas = 1;
    storageClassName = "longhorn";
    useProbes = false;
  };
}
