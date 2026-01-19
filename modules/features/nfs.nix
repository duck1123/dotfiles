{ ... }:
{
  flake.types.generic.feature-options.nfs =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "nfs feature";

  flake.modules.nixos.nfs-feature =
    { config, lib, ... }:
    {
      config = lib.mkIf config.host.features.nfs.enable {
        services.nfs.server = {
          enable = false;
          statdPort = 4000;
          lockdPort = 4001;
          mountdPort = 4002;

          exports = ''
            /mnt/nfs    *(ro,insecure,all_squash)
          '';
        };
      };
    };
}
