{ ... }:
{
  perSystem =
    { pkgs, ... }:
    let
      site = pkgs.buildNpmPackage {
        pname = "duck1123-site";
        version = "0.1.0";
        src = ../../applications/duck1123-site;
        # nix build .#duck1123-site  →  first failure shows the correct hash
        npmDepsHash = "sha256-zkrWShQU8Jz6vh+DRL0ILyVP83yl2TUAr/7eZTpcjig=";

        installPhase = ''
          mkdir -p $out
          cp -r dist/. $out/
        '';
      };
    in
    {
      # nix build .#duck1123-site
      packages.duck1123-site = site;

      # nix build .#duck1123-runtime
      # symlinkJoin of the built site + python3 to run applications/duck1123's
      # server.py -- this is what applications/duck1123/default.nix embeds the
      # store path of directly into its nix-csi volumeAttributes (per-system
      # storePath, not a nixExpr -- see that file's comment for why).
      packages.duck1123-runtime = pkgs.symlinkJoin {
        name = "duck1123-runtime";
        paths = [
          pkgs.python3
          site
        ];
      };
    };
}
