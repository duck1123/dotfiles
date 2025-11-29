{ ... }: {
  flake.types.generic.feature-options.bluetooth = { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in simpleFeature "bluetooth feature";

  flake.modules.nixos.bluetooth-feature = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.bluetooth.enable {
      hardware.bluetooth.enable = true;
    };
  };
}

