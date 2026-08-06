{ config, secrets, ... }:
{
  services.homarr = {
    enable = true;
    hostAffinity = "edgenix";

    ingress = {
      domain = "homarr.${config.devDefaults.homeDomain}";
      ingressClassName = "traefik";
      clusterIssuer = config.devDefaults.clusterIssuer;
      tls.enable = true;
    };

    secretEncryptionKey = secrets.homarr.secretEncryptionKey;
    storageClassName = "longhorn";
  };
}
