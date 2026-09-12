{ config, secrets, ... }:
{
  services.windmill = {
    enable = true;
    # hostAffinity = "nixmini";
    image = "ghcr.io/windmill-labs/windmill-full:latest";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;

    databaseTarget = "postgresql";
    database = {
      inherit (secrets.windmill.database) username password;
    };

    storageClassName = "longhorn";
    replicas = 1;

    inherit (secrets.windmill) superadminSecret superadminEmail superadminPassword;
  };
}
