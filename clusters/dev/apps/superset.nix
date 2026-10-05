{ secrets, config, ... }:
{
  services.superset = {
    admin = {
      username = (secrets.superset or { }).admin.username or "admin";
      email = (secrets.superset or { }).admin.email or "admin@superset.local";
      password = (secrets.superset or { }).admin.password or "";
    };

    database.password = (secrets.superset or { }).database.password or "";
    databaseTarget = "postgresql";
    enable = false;

    homepage.group = "Analytics";
    ingress.tls.enable = true;
    ingressProvider = "traefik-lan";
    redis.password = secrets.redis.password;
    secretKey = (secrets.superset or { }).secretKey or "";

    reportingConnections = map (db: {
      inherit (db) name username password;
      inherit (config.databaseProviders.postgresql) host port;
    }) config.services.postgresql.extraDatabases;
  };
}
