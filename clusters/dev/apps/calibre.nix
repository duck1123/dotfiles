{ config, ... }:
{
  services.calibre = {
    enable = false;

    ingressProvider = "traefik-lan";

    storageClassName = "longhorn";
  };
}
