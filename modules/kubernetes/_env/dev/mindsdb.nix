{ config, ... }:
{
  services.mindsdb = {
    enable = false;

    ingressProvider = "traefik-lan";
  };
}
