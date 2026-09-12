{ secrets, config, ... }:
{
  services.superset = {
    # Flip on once `superset.database.password` / `superset.secretKey` / `superset.admin.*`
    # are populated in secrets.enc.yaml (see `nur secrets edit`) and the matching entry
    # exists in env/dev/postgresql.nix's extraDatabases -- see IMAGE-VERSIONS.md /
    # applications/superset.nix for the exact keys expected.
    enable = true;

    databaseTarget = "postgresql";
    # Own alphanumeric-only password -- Superset's `superset db upgrade` runs through
    # Alembic/ConfigParser the same way securo's does (see env/dev/securo.nix), so it can't
    # share the punctuation-heavy databaseProviders.postgresql password either.
    database.password = (secrets.superset or { }).database.password or "";

    redis.password = secrets.redis.password;

    secretKey = (secrets.superset or { }).secretKey or "";

    admin = {
      username = (secrets.superset or { }).admin.username or "admin";
      email = (secrets.superset or { }).admin.email or "admin@superset.local";
      password = (secrets.superset or { }).admin.password or "";
    };

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Apps";

    # One SQL Lab connection per database on the shared postgres instance (Postgres has
    # no cross-database queries, so one connection can't cover all of them -- see
    # applications/superset.nix). Sourced from postgresql.nix's extraDatabases so new
    # app databases automatically get a connection registered on the next `nur switch`.
    reportingConnections = map (db: {
      inherit (db) name username password;
      inherit (config.databaseProviders.postgresql) host port;
    }) config.services.postgresql.extraDatabases;
  };
}
