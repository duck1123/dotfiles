{ config, secrets, ... }:
{
  services.homarr = {
    enable = true;
    hostAffinity = "edgenix";

    ingress = {
      domain = "homarr.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
      clusterIssuer = "tailscale";
    };

    secretEncryptionKey = secrets.homarr.secretEncryptionKey;
    storageClassName = "longhorn";
  };
}
