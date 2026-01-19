{ ... }:
let
  feature-name = "chat";
in
{
  flake.types.generic.feature-options.${feature-name} =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "${feature-name} feature";

  flake.modules.homeManager.${feature-name} =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf config.host.features.${feature-name}.enable {
        home.packages = with pkgs; [
          discord
          telegram-desktop
        ];
      };
    };

  flake.modules.nixos.${feature-name} =
    { config, lib, ... }:
    {
      config = lib.mkIf config.host.features.${feature-name}.enable { };
    };
}
