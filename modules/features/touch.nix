{ ... }: {
  flake.types.generic.feature-options.touch = { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in simpleFeature "touch feature";

  flake.modules.nixos.touch-feature = { config, lib, ... }: {
    config = lib.mkIf config.host.features.touch.enable {
      services.libinput.enable = true;
    };
  };
}

