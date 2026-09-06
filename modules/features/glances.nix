_: {
  flake.types.generic.feature-options.glances =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "glances feature";

  flake.modules.nixos.glances-feature =
    { config, lib, ... }:
    {
      config = lib.mkIf config.host.features.glances.enable {
        services.glances = {
          enable = true;
          openFirewall = true;
        };
      };
    };
}
