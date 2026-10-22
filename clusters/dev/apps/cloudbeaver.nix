{ config, ... }:
{
  services.cloudbeaver = {
    hostAffinity = "edgenix";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Database";

    storageClassName = "longhorn";
  };
}
