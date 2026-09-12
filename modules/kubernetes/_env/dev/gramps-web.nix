{ secrets, ... }:
{
  services.gramps-web = {
    enable = true;

    redis = {
      host = "redis.redis";
      port = 6379;
      password = secrets.redis.password;
    };

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides = {
      users.volumeHandle = "pvc-e13b7369-d016-4516-a7e0-135610b7b749";
      index.volumeHandle = "pvc-e22a2754-8e27-4e49-a1e2-074974ed07c8";
      thumbcache.volumeHandle = "pvc-adc04280-f619-48cf-b8ce-b7dbe015e01a";
      cache.volumeHandle = "pvc-49f888aa-c2b9-4a5a-9b6f-ee96c10f8e6a";
      secret.volumeHandle = "pvc-c604575b-fd8f-4597-b3f5-dd1a51a1aef4";
      db.volumeHandle = "pvc-c70d1079-d1bb-4af0-9993-dab06212b2ae";
      media.volumeHandle = "pvc-71a522a9-32bb-409d-b63d-e4c4555784b9";
    };
  };
}
