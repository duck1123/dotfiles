{ lib, ... }:
let
  mkNasMounts =
    {
      ip,
      shares,
    }:
    lib.listToAttrs (
      map (share: {
        name = "/mnt/${lib.toLower share}";
        value = {
          device = "${ip}:/volume1/${share}";
          fsType = "nfs";
          options = [
            "nfsvers=3"
            "rw"
            "soft"
            "noexec"
            "timeo=600"
            "retrans=2"
            "_netdev"
          ];
        };
      }) shares
    );
in
{
  flake.lib.nas-mounts = mkNasMounts;
}
