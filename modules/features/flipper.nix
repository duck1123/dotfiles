{ ... }: {
  flake.types.generic.feature-options.flipper = { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in simpleFeature "flipper feature";

  flake.modules.homeManager.flipper = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.flipper.enable {
      home.packages = with pkgs; [ qFlipper ];
    };
  };

  flake.modules.nixos.flipper = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.flipper.enable {
      hardware.flipperzero.enable = true;
    };
  };
}
