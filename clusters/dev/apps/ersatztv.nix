{ config, ... }:
{
  services.ersatztv = {
    # logLevel = "Debug";
    hostAffinity = "edgenix";

    ingressProvider = "traefik-lan";

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}/Videos";
    };

    enableGPU = true;
  };
}
