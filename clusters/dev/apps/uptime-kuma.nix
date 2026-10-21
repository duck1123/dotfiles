{ config, ... }:
{
  services.uptime-kuma = {
    enable = false;
    storageClassName = "longhorn";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Automation";

  };
}
