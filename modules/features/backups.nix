{ ... }:
{
  flake.types.generic.feature-options.backups =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "backups feature";

  flake.modules.homeManager.backups =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf config.host.features.backups.enable {
        home.packages = with pkgs; [
          borgmatic
          deja-dup
          duplicati
          restic
        ];
      };
    };
}
