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
        --prefix PATH : ${pkgs.lib.makeBinPath [
          pkgs.curl
          pkgs.libxml2.bin
          pkgs.xmlstarlet
          pkgs.gnugrep
        ]}
    '';
    meta.mainProgram = "soap";
  };

  nur-taskrunner = pkgs.rustPlatform.buildRustPackage {
    pname = "nur";
    version = "0.24.1+0.112.2";
    src = pkgs.fetchFromGitHub {
      owner = "nur-taskrunner";
      repo = "nur";
      rev = "v0.24.1+0.112.2";
      hash = "sha256-PNZQQhiV2j0THSlfTYM6yfAoZkjuIb6YtwY2Q9r7wfw=";
    };
    cargoHash = "sha256-AD9/0Byf18aPraSMpsiXzgADs+tbhjlQqBTOn87buJY=";
    meta.mainProgram = "nur";
  };

  ci-home = pkgs.symlinkJoin {
    name = "ci-home";
    paths = [
      inputs.self.homeConfigurations."duck@inspernix".activationPackage
      inputs.self.homeConfigurations."duck@nasnix".activationPackage
      inputs.self.homeConfigurations."duck@powerspecnix".activationPackage
      inputs.self.homeConfigurations."deck@steamdeck".activationPackage
      inputs.self.homeConfigurations."drenfer@VAVIRL-PW0BWNQ8".activationPackage
    ];
  };

  ci-os = pkgs.symlinkJoin {
    name = "ci-os";
    paths = [
      inputs.self.nixosConfigurations.inspernix.config.system.build.toplevel
      inputs.self.nixosConfigurations.nasnix.config.system.build.toplevel
      inputs.self.nixosConfigurations.powerspecnix.config.system.build.toplevel
    ];
  };

  ci = pkgs.symlinkJoin {
    name = "ci";
    paths = [
      ci-home
      ci-os
    ];
  };
in
{
  perSystem =
    { system, ... }:
    if system == "x86_64-linux" then
      {
        packages = {
          inherit
            ci
            ci-home
            ci-os
            nur-taskrunner
            soap-cli
            windmill-cli
            ;
        };
      }
    else
      { };
}
