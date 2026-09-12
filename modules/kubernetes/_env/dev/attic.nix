{ config, secrets, ... }:
{
  services.attic = {
    enable = true;

    homepage.group = "Storage";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;

    tokenHs256SecretBase64 = secrets.attic.tokenHs256SecretBase64;

    databaseTarget = "postgresql";

    storage = {
      # Shares garage's single auto-created "default" bucket (see
      # env/dev/garage.nix) rather than a dedicated "attic" bucket -- garage
      # only provisions one bucket on boot, and atticd's own keys are
      # content-hash-based so collision risk with other garage tenants
      # (e.g. xyops) is effectively nil.
      bucket = "default";
      # Must match garage's configured s3_region (see applications/garage.nix) --
      # "us-east-1" because that's what attic-server's presigned-download
      # codepath hardcodes regardless of this value, not because Garage cares.
      region = "us-east-1";
      # Use garage's ingress rather than the in-cluster garage.garage
      # ClusterIP: atticd generates presigned nar-download URLs against
      # whatever host this is, and garage.garage is only resolvable from
      # inside the pod network — external pulls (any LAN host, or any
      # nix-csi bootstrap that isn't itself a cluster pod) would 404 on
      # DNS. The ingress domain resolves everywhere.
      endpoint = "https://${config.services.garage.ingress.domain}";
      accessKey = (secrets.garage or { }).accessKey or "";
      secretKey = (secrets.garage or { }).secretKey or "";
    };
  };
}
