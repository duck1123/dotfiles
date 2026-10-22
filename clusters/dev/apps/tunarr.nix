{ config, ... }:
{
  services.tunarr = {
    enableGPU = true;
    # hostAffinity = "edgenix";
    resetDatabase = false;

    ingressProvider = "traefik-lan";

    nfs = {
      enable = false;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}";

      config = {
        enable = false;
        path = "${config.devDefaults.nasBase}/tunarr";
      };
    };

    storageClassName = "local-path";
  };
}
