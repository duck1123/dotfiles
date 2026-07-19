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
      patched = pkgs.extend (
        final: prev: {
          openldap = prev.openldap.overrideAttrs { doCheck = false; };
          pkgsi686Linux = prev.pkgsi686Linux.extend (
            _: prev686: {
              openldap = prev686.openldap.overrideAttrs { doCheck = false; };
            }
          );
        }
      );
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
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf config.host.features.gaming.enable {
        programs.steam.enable = true;

        # Lets prebuilt/non-nixpkgs game binaries (GOG/Lutris installs, etc.)
        # dynamically link against a normal FHS-ish library set on NixOS.
        programs.nix-ld = {
          enable = true;
          libraries = with pkgs; [
            alsa-lib
            fontconfig
            freetype
            libGL
            libGLU
            libjpeg
            libpng
            libpulseaudio
            libxkbcommon
            openssl
            SDL2
            SDL2_image
            SDL2_mixer
            SDL2_ttf
            stdenv.cc.cc
            vulkan-loader
            zlib
            libx11
            libxcomposite
            libxcursor
            libxdamage
            libxext
            libxfixes
            libxi
            libxinerama
            libxrandr
            libxrender
            libxtst
            libxxf86vm
          ];
        };
      };
    };
}
