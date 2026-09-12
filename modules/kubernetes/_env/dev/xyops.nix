{ secrets, ... }:
{
  services.xyops = {
    enable = true;

    databaseTarget = "postgresql";

    s3 = {
      # Shared garage bucket/credentials (see env/dev/garage.nix) -- keyPrefix
      # namespaces xyOps' objects within it rather than provisioning a
      # dedicated bucket, since garage only auto-creates one bucket on boot.
      bucket = "default";
      keyPrefix = "xyops/";
      accessKey = (secrets.garage or { }).accessKey or "";
      secretKey = (secrets.garage or { }).secretKey or "";
    };

    ingressProvider = "traefik-lan";
    homepage.group = "Automation";

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides.data.volumeHandle = "pvc-8c803a5c-b039-4ed1-bcec-6726d2f8276b";
  };
}
