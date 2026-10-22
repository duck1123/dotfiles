{ config, ... }:
{
  services.uptime-kuma = {
    storageClassName = "longhorn";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Automation";
  };
}
