{ secrets, ... }:
{
  services.redis = {
    enable = true;
    hostAffinity = "edgenix";
    password = secrets.redis.password;
    replicas = 1;
    repairAof = false;

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md.
    volumeOverrides.data.volumeHandle = "pvc-3432cb71-7a05-4ef4-b957-20ba12c4ab8d";
  };
}
