{ ... }:
{
  flake.types.generic.feature-options.flipper =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "flipper feature";

  flake.modules.homeManager.flipper =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf config.host.features.flipper.enable {
        home.packages = with pkgs; [ qFlipper ];
      };
    };

  flake.modules.nixos.flipper =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf config.host.features.flipper.enable {
        hardware.flipperzero.enable = true;
      };
    };
}
