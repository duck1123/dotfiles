{ ... }:
{
  flake.types.generic.feature-options.xserver =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "xserver feature";

  flake.modules.nixos.xserver-feature =
    { config, lib, ... }:
    {
      config = lib.mkIf config.host.features.xserver.enable {
        services.xserver = {
          enable = true;

          xkb = {
            layout = "us";
            variant = "";
          };
        };

        xdg.portal = {
          enable = true;
          config.common.default = "*";
        };
      };
    };
}
