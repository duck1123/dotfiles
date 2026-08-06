{ config, secrets, ... }:
{
  services.longhorn = {
    enable = true;
    backupTarget = "s3://longhorn@us-east-1/";

    backupTargetCredential = {
      accessKey = secrets.rustfs.accessKey;
      secretKey = secrets.rustfs.secretKey;
      endpoint = "http://rustfs-svc.rustfs:9000";
    };

    ingress = {
      clusterIssuer = config.devDefaults.clusterIssuer;
      domain = "longhorn.${config.devDefaults.homeDomain}";
      ingressClassName = "traefik";
      tls.enable = true;
    };
  };
}
