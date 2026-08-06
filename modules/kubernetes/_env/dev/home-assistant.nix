{ config, ... }:
{
  services.home-assistant = {
    enable = true;
    # hostAffinity = "edgenix";

    # https://github.com/AiDot-Development-Team/hass-AiDot
    installAidot.enable = true;

    ingress = {
      domain = "home-assistant.${config.devDefaults.homeDomain}";
      ingressClassName = "traefik";
      clusterIssuer = config.devDefaults.clusterIssuer;
      tls.enable = true;
    };

    storageClassName = "longhorn";
  };
}
