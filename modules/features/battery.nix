{ ... }:
{
  flake.types.generic.feature-options.battery =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "battery feature";

  flake.modules.nixos.battery-feature =
    { config, lib, ... }:
    {
      config = lib.mkIf config.host.features.battery.enable {
        services.upower.enable = true;
      };
    };
}
