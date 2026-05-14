{ ... }:
{
  flake.types.generic.feature-options.zen-browser =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "zen-browser feature";

  flake.modules.homeManager.zen-browser =
    { config, lib, ... }:
    {
      config = lib.mkIf config.host.features.zen-browser.enable {
        programs.zen-browser = {
          enable = true;
          policies = {
            DisableAppUpdate = true;
            DisableTelemetry = true;
          };
        };

        stylix.targets.zen-browser.profileNames = lib.mkIf config.host.features.stylix.enable [ "default" ];
      };
    };
}
