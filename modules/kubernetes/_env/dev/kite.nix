{ config, secrets, ... }:
{
  services.kite = {
    inherit (secrets.kite) encryptKey jwtSecret;
    enable = true;
    hostAffinity = "edgenix";
    storageClassName = "longhorn";

    ingress = {
      domain = "kite.${config.devDefaults.homeDomain}";
      clusterIssuer = config.devDefaults.clusterIssuer;
      ingressClassName = "traefik";
      tls.enable = true;
    };
  };
}
