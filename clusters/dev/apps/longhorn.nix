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

    homepage.group = "Storage";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
  };
}
