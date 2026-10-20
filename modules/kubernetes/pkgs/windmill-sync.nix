_: {
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
        src = ./wmill-cli;
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
      packages = {
        # nix build .#wmill-cli
        inherit wmill-cli;

        # nix build .#windmill-sync-bundle
        # symlinkJoin of the wmill CLI, the shell tools the sync job's script
        # needs, and resources/apps/windmill/wmill (this repo's declarative
        # resources/scripts/flows/variables tree) at share/windmill-wmill --
        # same self-referential-flake bundling as appTemplates/duck1123.nix's
        # site+python3. The
        # bundle is built locally when `nur switch k8s` evaluates the manifests
        # and pinned by store path, so it carries the git-tracked working tree
        # (`git add` is enough; no push to the remote needed).
        #
        # Only f/ and wmill.yaml go in. u/ holds drafts pushed by hand
        # (e.g. u/duck/inbox_review, a raw app that needs node_modules to
        # bundle); `wmill sync push` re-pushed it despite `includes: f/**`,
        # replacing its working bundle with a broken one on every sync.
        windmill-sync-bundle = pkgs.symlinkJoin {
          name = "windmill-sync-bundle";
          paths = [
            wmill-cli
            pkgs.bash
            pkgs.curl
            pkgs.jq
            pkgs.coreutils
            (pkgs.runCommand "windmill-wmill-config" { } ''
              mkdir -p $out/share/windmill-wmill
              cp -r ${../../../resources/apps/windmill/wmill}/{f,wmill.yaml} $out/share/windmill-wmill/
            '')
          ];
        };

        # nix build .#windmill-worker-native-tools
        # Mounted at /nix inside the windmill-worker-native container
        # (appTemplates/windmill.nix) so "native"-tagged Windmill
        # scripts have a small toolset to work with beyond the windmill-labs
        # image itself. Resolved via the same storePath convention as
        # windmill-sync-bundle above -- see that package's comment, and
        # appTemplates/duck1123.nix's duck1123Runtime, for why this is
        # a real flake package rather than a nixExpr string.
        windmill-worker-native-tools = pkgs.buildEnv {
          name = "windmill-worker-native-tools";
          paths = with pkgs; [
            bash
            coreutils
            git
            curl
            jq
            nushell
            nix
          ];
        };
      };
    };
}
