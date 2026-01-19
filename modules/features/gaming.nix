{ ... }:
{
  flake.types.generic.feature-options.gaming =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "gaming feature";

  flake.modules.homeManager.gaming =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf config.host.features.gaming.enable {
        home.packages = with pkgs; [
          dolphin-emu
          # heroic
          itch
          lutris
          # nexusmods-app
          protontricks
          satisfactorymodmanager
          wine
        ];
      };
    };

  flake.modules.nixos.gaming-feature =
    { config, lib, ... }:
    {
      config = lib.mkIf config.host.features.gaming.enable {
        programs.steam.enable = true;
      };
    };
}
