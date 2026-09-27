{ config, ... }:
{
  services.dozzle = {
    enable = false;
    hostAffinity = "nasnix";

    ingressProvider = "traefik-lan";
  };
}
