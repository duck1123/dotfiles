{ config, secrets, ... }:
{
  services.homarr = {
    # hostAffinity = "edgenix";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;

    monitoring.autokuma.enable = true;
    secretEncryptionKey = secrets.homarr.secretEncryptionKey;
    storageClassName = "longhorn";
  };
}
