{ config, secrets, ... }:
{
  services.affine = {
    enable = false;
    # hostAffinity = "edgenix";

    databaseTarget = "postgresql";

    redis = {
      host = "redis.redis";
      port = 6379;
      password = secrets.redis.password;
    };

    serverExternalUrl = "https://affine.${config.devDefaults.homeDomain}";

    ingressProvider = "traefik-lan";

    storageClassName = "longhorn";

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides = {
      storage.volumeHandle = "pvc-4e946647-f3c7-475c-8475-8cae5ca47eb4";
      affine-config.volumeHandle = "pvc-da6fcdf7-7372-43e3-8c87-5eee4f2ab1a2";
    };
  };
}
