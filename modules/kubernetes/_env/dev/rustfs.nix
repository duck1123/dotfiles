{ config, secrets, ... }:
{
  services.rustfs = {
    accessKey = (secrets.rustfs or { }).accessKey or "";
    enable = true;
    hostAffinity = "nasnix";

    ingress = {
      domain = "rustfs.${config.devDefaults.homeDomain}";
      api-domain = "api-rustfs.${config.devDefaults.homeDomain}";
      ingressClassName = "traefik";
      clusterIssuer = config.devDefaults.clusterIssuer;
      tls.enable = true;
    };

    mode = "standalone";

    # NFS-backed rather than Longhorn: rustfs is Longhorn's own S3 backup
    # target, so its storage shouldn't depend on Longhorn itself.
    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}/LonghornBackups";
    };

    secretKey = (secrets.rustfs or { }).secretKey or "";
  };
}
