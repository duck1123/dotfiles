{ inputs, ... }:
let
  pkgs = import inputs.nixpkgs {
    config.allowUnfree = true;
    system = "x86_64-linux";
  };

  windmill-cli =
    let
      version = "1.691.0";
    in
    pkgs.stdenv.mkDerivation {
      pname = "windmill-cli";
      inherit version;
      src = pkgs.fetchurl {
        url = "https://registry.npmjs.org/windmill-cli/-/windmill-cli-${version}.tgz";
        hash = "sha256-EPYT4k54o863TZjqGlpw/bPLSx7liYbgZV6y9N+vFxU=";
      };
      sourceRoot = "package";
      nativeBuildInputs = [ pkgs.makeWrapper ];
      dontBuild = true;
      installPhase = ''
        mkdir -p $out/lib/windmill-cli
        cp -r . $out/lib/windmill-cli/
        mkdir -p $out/bin
        makeWrapper ${pkgs.nodejs}/bin/node $out/bin/wmill \
          --add-flags "$out/lib/windmill-cli/esm/main.js"
      '';
      meta.mainProgram = "wmill";
    };

  # Caldera's desktop Plexamp-style player. Upstream only ships a Flatpak
  # bundle, so unpack it and patch the binary to run natively.
  caldera-amp =
    let
      version = "1.1.1";
    in
    pkgs.stdenv.mkDerivation {
      pname = "caldera-amp";
      inherit version;
      src = pkgs.fetchurl {
        url = "https://releases.caldera.homes/music/amp/linux/v${version}/CalderaAmp-${version}-x64.flatpak";
        hash = "sha256-74XAIg5WcWh0Sza/myCKtncrU/1mHp5daFK/4qDODlw=";
      };
      nativeBuildInputs = with pkgs; [
        autoPatchelfHook
        flatpak
        ostree
        wrapGAppsHook4
      ];
      buildInputs = with pkgs; [
        alsa-lib
        brotli
        cairo
        gdk-pixbuf
        glib
        gtk4
        libGL
        libidn2
        libvdpau
        nghttp2
        openssl
        pipewire
        stdenv.cc.cc.lib
        zlib
        zstd
      ];
      unpackPhase = ''
        export HOME=$TMPDIR
        ostree init --repo=repo --mode=archive-z2
        flatpak build-import-bundle repo $src
        ostree checkout --repo=repo -U app/com.caldera.CalderaAmp/x86_64/master app
      '';
      dontBuild = true;
      installPhase = ''
        mkdir -p $out/bin $out/lib/caldera-amp $out/share
        cp app/files/bin/caldera-amp $out/bin/
        # Bundled ffmpeg 8 libs, so the binary gets the exact ABI it was built against
        cp -P app/files/lib/lib*.so* $out/lib/caldera-amp/
        cp -r app/files/share/{applications,caldera-amp,icons,metainfo} $out/share/

        # Besides /app and /usr, it finds its data by walking up from the
        # executable to its source-tree layout
        data=$out/share/caldera-amp
        mkdir -p $out/platform/linux "$out/vendor/MilkDrop Data"
        ln -s $data/opengl_shaders $out/platform/linux/opengl_shaders
        ln -s $data/fonts $out/vendor/fonts
        ln -s $data/presets "$out/vendor/MilkDrop Data/presets"
      '';
      meta.mainProgram = "caldera-amp";
    };

  # Caldera's headless Plex Companion player. Upstream only publishes a
  # `latest` tarball, so this hash breaks on every release; bump it then.
  caldera-music = pkgs.stdenv.mkDerivation {
    pname = "caldera-music";
    version = "1.1.0";
    src = pkgs.fetchurl {
      url = "https://releases.caldera.homes/music/headless/latest/caldera-music-linux-x86_64.tar.gz";
      hash = "sha256-R2giaoB3vwHDfK9iL9wvMXsCAYqV1fwLH3hntB/aGJ8=";
    };
    sourceRoot = ".";
    nativeBuildInputs = [ pkgs.autoPatchelfHook ];
    buildInputs = with pkgs; [
      alsa-lib
      stdenv.cc.cc.lib
      zlib
    ];
    dontBuild = true;
    installPhase = ''
      mkdir -p $out/bin $out/lib/caldera-music
      cp bin/caldera-music $out/bin/
      cp -P lib/lib*.so* $out/lib/caldera-music/
    '';
    meta.mainProgram = "caldera-music";
  };

  soap-cli = pkgs.stdenv.mkDerivation {
    pname = "soap-cli";
    version = "1.3";
    src = pkgs.fetchFromGitHub {
      owner = "pmamico";
      repo = "soap-cli";
      rev = "v1.3";
      hash = "sha256-2YIzeHMhF8HQDI04olqZ6q8z3+L66VpqlyrClNp2BLQ=";
    };
    nativeBuildInputs = [ pkgs.makeWrapper ];
    dontBuild = true;
    installPhase = ''
      mkdir -p $out/bin
      cp src/soap $out/bin/soap
      chmod +x $out/bin/soap
      wrapProgram $out/bin/soap \
        --prefix PATH : ${
          pkgs.lib.makeBinPath [
            pkgs.curl
            pkgs.libxml2.bin
            pkgs.xmlstarlet
            pkgs.gnugrep
          ]
        }
    '';
    meta.mainProgram = "soap";
  };

  ngit =
    let
      version = "2.6.2";
      src = pkgs.fetchCrate {
        pname = "ngit";
        inherit version;
        hash = "sha256-SxPYYgfPf13GxFGdECAMh08J9vfGxIEXhIc28AbWJDc=";
      };
    in
    pkgs.rustPlatform.buildRustPackage {
      pname = "ngit";
      inherit version src;
      cargoLock.lockFile = "${src}/Cargo.lock";
      nativeBuildInputs = [ pkgs.pkg-config ];
      buildInputs = [ pkgs.openssl ];
      doCheck = false;
      meta.mainProgram = "ngit";
    };

  # Lets the back side button drag windows like Mod+LMB, which niri hardcodes
  # and can't bind from config.
  niri = pkgs.niri.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [ ../../patches/niri-side-button-drag.patch ];
  });

  pnu = pkgs.writeShellApplication {
    name = "pnu";
    runtimeInputs = [ pkgs.nushell ];
    text = ''
      CONFIG_DIR="''${XDG_CONFIG_HOME:-$HOME/.config}/nushell"
      CONFIG="$CONFIG_DIR/config.nu"
      ENV_CONFIG="$CONFIG_DIR/env.nu"
      SCRIPT="$PWD/scripts/nur.nu"

      TMP=$(mktemp --suffix=.nu)
      trap 'rm -f "$TMP"' EXIT

      if [ -f "$SCRIPT" ]; then
        printf 'source "%s"\noverlay use "%s"\n' "$CONFIG" "$SCRIPT" > "$TMP"
      else
        printf 'source "%s"\n' "$CONFIG" > "$TMP"
      fi

      exec nu --env-config "$ENV_CONFIG" --config "$TMP" "$@"
    '';
  };

in
{
  perSystem =
    { system, ... }:
    if system == "x86_64-linux" then
      {
        packages = {
          inherit
            caldera-amp
            caldera-music
            ngit
            niri
            pnu
            soap-cli
            windmill-cli
            ;
        };
      }
    else
      { };
}
