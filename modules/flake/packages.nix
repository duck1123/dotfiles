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
            windmill-cli
            ;
        };
      }
    else
      { };
}
