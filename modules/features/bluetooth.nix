{ ... }: {
  flake.types.generic.feature-options.bluetooth = { inputs, lib }:
    let inherit (inputs.self.types.generic) simpleFeature;
    in simpleFeature { inherit inputs lib; } "bluetooth feature";

  flake.modules.nixos.bluetooth-feature = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.bluetooth.enable {
      hardware.bluetooth.enable = true;
    };
  };
}

