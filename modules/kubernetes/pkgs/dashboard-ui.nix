_: {
  perSystem =
    { pkgs, ... }:
    let
      # Static Storybook build of resources/apps/dashboard-ui (the shared UI
      # components). The unit tests and the typecheck run as the check phase,
      # so a broken component fails the build rather than the deploy.
      storybook = pkgs.buildNpmPackage {
        pname = "dashboard-ui-storybook";
        version = "0.1.0";
        src = ../../../resources/apps/dashboard-ui;
        # `nur ui hash` rewrites this after package-lock.json changes
        npmDepsHash = "sha256-lhub0vWqIJy9NvEMuzGUZTkoQw34yof1B9/THFdiTbc=";

        npmBuildScript = "build-storybook";
        env.STORYBOOK_DISABLE_TELEMETRY = "1";

        doCheck = true;
        checkPhase = ''
          runHook preCheck
          npm run typecheck
          npm test
          runHook postCheck
        '';

        installPhase = ''
          runHook preInstall
          cp -r storybook-static $out
          runHook postInstall
        '';
      };
    in
    {
      # nix build .#dashboard-ui-storybook
      packages.dashboard-ui-storybook = storybook;

      # nix build .#storybook-runtime
      # What appTemplates/storybook.nix mounts through nixkube: python3 to
      # serve the files, and the site at share/storybook (kept out of the
      # root so python's own lib/ isn't served alongside it).
      packages.storybook-runtime = pkgs.symlinkJoin {
        name = "storybook-runtime";
        paths = [
          pkgs.python3
          (pkgs.runCommand "storybook-site" { } ''
            mkdir -p $out/share
            ln -s ${storybook} $out/share/storybook
          '')
        ];
      };
    };
}
