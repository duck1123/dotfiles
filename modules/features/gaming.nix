_: {
  features.gaming = {
    homeManager =
      { pkgs, ... }:
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

    nixos =
      { pkgs, ... }:
      {
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
