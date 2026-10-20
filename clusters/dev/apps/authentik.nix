{ secrets, ... }:
{
  services.authentik = {
    inherit (secrets.authentik) secret-key;
    enable = false;

    ingressProvider = "traefik-dev";

    databaseTarget = "postgresql";
    database.password = secrets.authentik.postgresql.password;
  };
}
