{ config, ... }:
{
  services.mindsdb = {
    ingressProvider = "traefik-lan";
  };
}
