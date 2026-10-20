{ lib, ... }:
{
  options.databaseProviders = lib.mkOption {
    description = ''
      Named database providers. Keyed by name (i.e. the value used in
      services.<name>.database.target). mkArgoApp uses these as mkDefault
      values for the database host, port, username, and password.

      usernameFor/passwordFor are functions of the app name, since some
      providers hand every app its own role/secret (one role per app) while
      others hand every app the same shared role/secret.
    '';
    type = lib.types.attrsOf (
      lib.types.submodule {
        options = {
          kind = lib.mkOption {
            description = "Database engine.";
            type = lib.types.enum [
              "postgresql"
              "mariadb"
            ];
          };
          app = lib.mkOption {
            description = ''
              The app (services.<app>) that creates databases registered on
              this provider, or null if nothing in the cluster does (e.g. a
              server running directly on a node).
            '';
            type = lib.types.nullOr lib.types.str;
            default = null;
          };
          superuser = lib.mkOption {
            description = ''
              The provider's admin role. A database whose role is this one
              still gets created, but the role itself is never created,
              re-passworded or made the owner.
            '';
            type = lib.types.nullOr lib.types.str;
            default = null;
          };
          host = lib.mkOption {
            description = "Database host (cluster DNS).";
            type = lib.types.str;
          };
          port = lib.mkOption {
            description = "Database port.";
            type = lib.types.port;
          };
          usernameFor = lib.mkOption {
            description = "Function from app name to the default database username for that app.";
            type = lib.types.functionTo lib.types.str;
          };
          passwordFor = lib.mkOption {
            description = "Function from app name to the default database password for that app.";
            type = lib.types.functionTo lib.types.str;
          };
        };
      }
    );
    default = { };
  };
}
