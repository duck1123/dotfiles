{ config, ... }:
{
  services.homer = {
    codeserver.ingress = {
      domain = "codeserver.${config.devDefaults.homeDomain}";
      enable = true;
    };

    enable = false;

    ingressProvider = "traefik-lan";
  };
}
