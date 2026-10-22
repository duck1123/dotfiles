{ config, secrets, ... }:
{
  services.mealie = {
    # hostAffinity = "edgenix";
    image = "ghcr.io/mealie-recipes/mealie:v3.25.1";

    databaseTarget = "postgresql";
    database = {
      enable = true;
      password = secrets.mealie.databasePassword;
    };

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;

    storageClassName = "longhorn";
  };
}
