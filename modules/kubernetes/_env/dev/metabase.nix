{ secrets, config, ... }:
{
  services.metabase = {
    enable = true;

    ingressProvider = "traefik-lan";

    # Flip these on by populating metabase.admin.password in secrets.enc.yaml (see
    # `nur secrets edit`) -- until then the register-connections job is skipped
    # entirely (see applications/metabase.nix).
    admin = {
      email = (secrets.metabase or { }).admin.email or "admin@metabase.local";
      password = (secrets.metabase or { }).admin.password or "";
    };

    # One entry per Postgres database to expose in Metabase, sourced the same way
    # as superset's reportingConnections (env/dev/superset.nix) -- proven out
    # against the live cluster with just "immich" first, now covers every
    # database on the shared postgresql instance automatically.
    reportingConnections = map (db: {
      inherit (db) name username password;
      inherit (config.databaseProviders.postgresql) host port;
    }) config.services.postgresql.extraDatabases;
  };
}
