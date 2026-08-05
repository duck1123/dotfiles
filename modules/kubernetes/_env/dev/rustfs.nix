{ config, secrets, ... }:
{
  services.rustfs = {
    accessKey = (secrets.rustfs or { }).accessKey or "";
    enable = true;
    hostAffinity = "nasnix";

    ingress = {
      domain = "rustfs.${config.devDefaults.tailDomain}";
      api-domain = "api-rustfs.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
      clusterIssuer = "tailscale";
      tls.enable = true;
    };

    mode = "standalone";
    secretKey = (secrets.rustfs or { }).secretKey or "";
    storageClassName = "longhorn";
  };
}
