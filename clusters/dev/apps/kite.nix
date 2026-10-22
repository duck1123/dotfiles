{ config, secrets, ... }:
{
  services.kite = {
    inherit (secrets.kite) encryptKey jwtSecret;
    hostAffinity = "edgenix";
    storageClassName = "longhorn";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
  };
}
