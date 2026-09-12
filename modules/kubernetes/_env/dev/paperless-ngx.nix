{ secrets, ... }:
{
  services.paperless-ngx = {
    enable = true;

    databaseTarget = "postgresql";

    inherit (secrets.paperless-ngx) adminUser adminPassword secretKey;

    redis = {
      host = "redis.redis";
      port = 6379;
      password = secrets.redis.password;
    };

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Notes";

    storageClassName = "longhorn";

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides = {
      data.volumeHandle = "pvc-a4fa895d-d3ee-46de-85ad-d25e2b4be3b2";
      media.volumeHandle = "pvc-9ac4dcc3-29d3-4acc-8b6d-8dc96d0ff256";
      export.volumeHandle = "pvc-867ee2c3-0479-48dd-8788-45cbc629bf59";
      consume.volumeHandle = "pvc-594d132e-e8f6-444a-988d-f649856d055a";
    };
  };
}
