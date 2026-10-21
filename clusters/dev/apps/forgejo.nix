{ secrets, ... }:
{
  services.forgejo = {
    admin = { inherit (secrets.forgejo.admin) password username; };
    enable = false;
    databaseTarget = "postgresql";
    ingressProvider = "traefik-lan";
    monitoring.autokuma.enable = true;

    # The postgres login forgejo connects with (not forgejo's own admin user).
    database = {
      username = "postgres";
      password = secrets.forgejo.postgresql.adminPassword;
    };

    storageClassName = "longhorn";

  };
}
