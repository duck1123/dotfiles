{ secrets, ... }:
{
  services.xyops = {
    enable = false;

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

  };
}
