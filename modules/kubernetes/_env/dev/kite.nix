{ config, secrets, ... }:
{
  services.kite = {
    inherit (secrets.kite) encryptKey jwtSecret;
    enable = true;
    hostAffinity = "edgenix";
    storageClassName = "longhorn";

    ingress = {
      domain = "kite.${config.devDefaults.tailDomain}";
      clusterIssuer = "tailscale";
      ingressClassName = "tailscale";
      tls.enable = true;
    };
  };
}
