{ config, ... }:
{
  services.home-assistant = {
    enable = true;
    # hostAffinity = "edgenix";

    # https://github.com/AiDot-Development-Team/hass-AiDot
    installAidot.enable = true;

    ingress = {
      domain = "home-assistant.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
      clusterIssuer = "tailscale";
      tls.enable = true;
    };

    storageClassName = "longhorn";
  };
}
