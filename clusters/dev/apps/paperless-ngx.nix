{ secrets, ... }:
{
  services.paperless-ngx = {
    enable = true;

    databaseTarget = "postgresql";

    inherit (secrets.paperless-ngx) adminUser adminPassword secretKey;

    redisTarget = "redis";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Organization";

    storageClassName = "longhorn";
  };
}
