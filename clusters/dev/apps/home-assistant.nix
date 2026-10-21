{ config, ... }:
{
  services.home-assistant = {
    enable = false;
    # hostAffinity = "edgenix";

    # https://github.com/AiDot-Development-Team/hass-AiDot
    installAidot.enable = true;

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;

    monitoring.autokuma.enable = true;
    storageClassName = "longhorn";

  };
}
