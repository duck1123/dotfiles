{ config, secrets, ... }:
{
  services.windmill = {
    enable = true;
    # hostAffinity = "nixmini";
    image = "ghcr.io/windmill-labs/windmill-full:latest";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Automation";

    databaseTarget = "postgresql";
    database = {
      inherit (secrets.windmill.database) username password;
    };

    storageClassName = "longhorn";
    replicas = 1;

    # nfsTarget defaults to "nas" and nfsSubPath defaults to "", so this mounts
    # the whole nasnix media export at /media in both worker deployments --
    # the same library every *arr app sees.
    nfs.enable = true;

    inherit (secrets.windmill) superadminSecret superadminEmail superadminPassword;

    # Valueless definitions live in resources/apps/windmill/wmill/f/fleetops/.
    secretVariables = [
      {
        path = "f/fleetops/youtube_api_key";
        value = secrets.youtube.apiKey;
      }
      {
        path = "f/fleetops/trilium_etapi_token";
        value = secrets.trilium.key;
      }
    ];

    # One Windmill resource per database on the shared postgres instance,
    # registered at f/fleetops/db_<name> -- sourced the same way as
    # superset's/metabase's reportingConnections (env/dev/superset.nix).
    reportingConnections = map (db: {
      inherit (db) name username password;
      inherit (config.databaseProviders.postgresql) host port;
    }) config.services.postgresql.extraDatabases;
  };
}
