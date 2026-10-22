{ config, ... }:
{
  services.kapowarr = {
    hostAffinity = "edgenix";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}";

      library = {
        enable = true;
        path = "${config.devDefaults.nasBase}/Books";
      };
    };

    storageClassName = "longhorn";
    vpn.enable = false;
  };
}
