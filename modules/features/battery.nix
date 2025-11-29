{ ... }: {
  flake.types.generic.feature-options.battery = { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in simpleFeature "battery feature";

  flake.modules.nixos.battery-feature = { config, lib, ... }: {
    config = lib.mkIf config.host.features.battery.enable {
      services.upower.enable = true;
    };
  };
}
