{ config, ... }:
{
  services.kapowarr = {
    enable = false;
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

    replicas = 1;
    storageClassName = "longhorn";
    vpn.enable = false;

  };
}
