{ config, secrets, ... }:
{
  services.nocodb = {
    allowLocalExternalDatabases = true;
    auth.jwtSecret = (secrets.nocodb or { }).jwtSecret or "";
    enable = false;

    homepage.group = "Database";

    ingressProvider = "traefik-lan";

    databaseTarget = "postgresql";
    database.password = (secrets.nocodb.postgresql or { }).password or secrets.postgresql.userPassword;

    redis = {
      host = "redis.redis";
      port = 6379;
      password = secrets.redis.password;
    };

    # Shared garage bucket/credentials (see env/dev/garage.nix) -- garage only
    # auto-creates one "default" bucket on boot (same bucket attic/xyops use).
    # Endpoint must be garage's ingress domain, not the in-cluster
    # garage.garage ClusterIP -- see the `storage.endpoint` option doc in
    # applications/nocodb.nix for why (presigned URLs go to the browser).
    storage = {
      enable = true;
      backend = "garage";
      bucketName = "default";
      endpoint = "https://${config.services.garage.ingress.domain}";
      region = "us-east-1";
      accessKey = (secrets.garage or { }).accessKey or "";
      secretKey = (secrets.garage or { }).secretKey or "";
    };

    publicUrl = "https://nocodb.${config.devDefaults.homeDomain}";

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides.data.volumeHandle = "pvc-31a2f2bc-8818-4500-ab0a-88f2db40d7b7";
  };
}
