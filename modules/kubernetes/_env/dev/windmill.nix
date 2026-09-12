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

    # One Windmill resource per database on the shared postgres instance,
    # registered at f/fleetops/db_<name> -- sourced the same way as
    # superset's/metabase's reportingConnections (env/dev/superset.nix).
    reportingConnections = map (db: {
      inherit (db) name username password;
      inherit (config.databaseProviders.postgresql) host port;
    }) config.services.postgresql.extraDatabases;
  };
}
