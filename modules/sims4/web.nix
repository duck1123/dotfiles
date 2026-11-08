# The Sims mod web UI that `sims web` serves on localhost: dashboard-ui's
# apps/sims page, built, plus its dependency-free Node server. Code only: the
# mod list comes from the private catalog at run time, never through the store.
_: {
  perSystem =
    { pkgs, self', ... }:
    {
      # nix build .#simsnix-web
      packages.simsnix-web = pkgs.buildNpmPackage {
        pname = "simsnix-web";
        version = "0.1.0";
        src = ../../resources/apps/dashboard-ui;
        # Same package-lock.json as the Storybook build (modules/kubernetes/pkgs/dashboard-ui.nix),
        # so its dependencies too, and `nur ui hash` keeps both up to date
        inherit (self'.packages.dashboard-ui-storybook) npmDeps;

        npmBuildScript = "build-sims";

        installPhase = ''
          runHook preInstall
          mkdir -p $out/share/simsnix-web
          cp -r dist-sims $out/share/simsnix-web/site
          cp apps/sims/server.mjs $out/share/simsnix-web/server.mjs
          runHook postInstall
        '';
      };
    };
}
