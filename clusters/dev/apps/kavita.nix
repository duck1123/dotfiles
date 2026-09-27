{ config, ... }:
{
  services.kavita = {
    enable = false;

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
  };
}
