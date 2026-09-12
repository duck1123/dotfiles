{ config, ... }:
{
  services.tempo = {
    enable = false;
    ingressProvider = "traefik-lan";
  };
}
