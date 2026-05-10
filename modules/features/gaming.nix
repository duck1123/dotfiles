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
    let
      patched = pkgs.extend (final: prev: {
        openldap = prev.openldap.overrideAttrs { doCheck = false; };
        pkgsi686Linux = prev.pkgsi686Linux.extend (_: prev686: {
          openldap = prev686.openldap.overrideAttrs { doCheck = false; };
        });
      });
    in
    {
      config = lib.mkIf config.host.features.gaming.enable {
        home.packages = with pkgs; [
          dolphin-emu
          # heroic
          itch
          patched.lutris

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
