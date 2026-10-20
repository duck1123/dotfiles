{ secrets, ... }:
{
  services.authentik = {
    inherit (secrets.authentik) secret-key;
    enable = false;

    ingressProvider = "traefik-dev";

    # Off until secrets.authentik exists: a targeted database is registered
    # (and created by postgresql) even while the app is disabled.
    # databaseTarget = "postgresql";
    # database.password = secrets.authentik.postgresql.password;
  };
}
