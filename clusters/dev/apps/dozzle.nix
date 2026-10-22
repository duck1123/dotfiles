{ config, ... }:
{
  services.dozzle = {
    hostAffinity = "nasnix";

    ingressProvider = "traefik-lan";
  };
}
