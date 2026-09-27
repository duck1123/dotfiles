{ config, ... }:
{
  services.spark = {
    enable = false;

    ingressProvider = "traefik-lan";
  };
}
