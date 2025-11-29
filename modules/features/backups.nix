{ ... }: {
  flake.types.generic.feature-options.backups = { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in simpleFeature "backups feature";

  flake.modules.homeManager.backups = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.backups.enable {
      home.packages = with pkgs; [ borgmatic deja-dup duplicati restic ];
    };
  };
}

