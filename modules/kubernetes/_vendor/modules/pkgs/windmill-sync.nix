{ ... }:
{
  perSystem =
    { pkgs, ... }:
    let
      # windmill-cli (the `wmill` binary) isn't in nixpkgs. Package the published
      # npm module directly: modules/pkgs/wmill-cli/{package.json,package-lock.json}
      # depend on nothing but windmill-cli@<version>, pinned the same way
      # IMAGE-VERSIONS.md pins container tags -- bump the version there and here
      # together, then `nix build .#wmill-cli` to get the new npmDepsHash.
      wmill-cli = pkgs.buildNpmPackage {
        pname = "wmill-cli";
        version = "1.719.0";
        src = ../../modules/pkgs/wmill-cli;
        # nix build .#wmill-cli  →  first failure shows the correct hash
        npmDepsHash = "sha256-noy8fkzzhNsAACto88WbsmPyrPVSpLCnspnbZVzkpSw=";
        dontNpmBuild = true;
        nativeBuildInputs = [ pkgs.makeWrapper ];
        installPhase = ''
          mkdir -p $out/bin
          cp -r node_modules $out/
          makeWrapper ${pkgs.nodejs}/bin/node $out/bin/wmill \
            --add-flags "$out/node_modules/windmill-cli/esm/main.js"
        '';
      };
    in
    {
      # nix build .#wmill-cli
      packages.wmill-cli = wmill-cli;

      # nix build .#windmill-sync-bundle
      # symlinkJoin of the wmill CLI, the shell tools the sync job's script
      # needs, and applications/windmill/wmill (this repo's declarative
      # resources/scripts/flows/variables tree) at share/windmill-wmill --
      # same self-referential-flake bundling as applications/duck1123's
      # site+python3 and applications/nostrarchives's compiled binary. Since
      # nix-csi fetches this flake fresh from GitHub, the synced content
      # always reflects the last-*pushed* commit, not local uncommitted edits.
      packages.windmill-sync-bundle = pkgs.symlinkJoin {
        name = "windmill-sync-bundle";
        paths = [
          wmill-cli
          pkgs.bash
          pkgs.curl
          pkgs.jq
          pkgs.coreutils
          (pkgs.runCommand "windmill-wmill-config" { } ''
            mkdir -p $out/share
            cp -r ${../../applications/windmill/wmill} $out/share/windmill-wmill
          '')
        ];
      };
    };
}
