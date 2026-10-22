{ config, ... }:
{
  services.tempo = {
    ingressProvider = "traefik-lan";
  };
}
