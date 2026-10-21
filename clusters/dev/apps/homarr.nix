{ config, secrets, ... }:
{
  services.homarr = {
    enable = false;
    # hostAffinity = "edgenix";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;

    monitoring.autokuma.enable = true;
    secretEncryptionKey = secrets.homarr.secretEncryptionKey;
    storageClassName = "longhorn";

  };
}
