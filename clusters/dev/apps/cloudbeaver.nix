{ config, ... }:
{
  services.cloudbeaver = {
    enable = false;
    hostAffinity = "edgenix";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Database";

    storageClassName = "longhorn";

  };
}
