{ config, ... }:
{
  services.demo = {
    enable = false;
    ingressProvider = "traefik-lan";
  };
}
