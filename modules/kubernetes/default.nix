{ inputs, ... }:
{
  imports = [
    # Pull in all lib functions (loadSecrets, fromYAML, mkArgoApp, etc.),
    # the nixidyApps option, all application modules, and the ageRecipients module.
    # Vendored from k3s-fleetops (modules/kubernetes/_vendor/) rather than
    # pulled via flake input -- see the consolidation plan's phase 5.
    ./_vendor/modules/flake/flake-parts.nix
    ./_vendor/modules/lib/fromYAML.nix
    ./_vendor/modules/lib/toYAML.nix
    ./_vendor/modules/lib/loadSecrets.nix
    ./_vendor/modules/lib/mkArgoApp.nix
    ./_vendor/modules/lib/mkPinnedVolume.nix
    ./_vendor/modules/lib/waitForGluetun.nix
    ./_vendor/modules/applications.nix
    ./_vendor/modules/sops.nix
    # modules/pkgs declares flake.packages some apps reference directly
    # (e.g. self.packages.<system>.duck1123-runtime) -- import-tree'd rather
    # than listed file-by-file since fleetops adds new ones over time and a
    # missing one fails evaluation deep inside an unrelated app's build.
    (inputs.import-tree ./_vendor/modules/pkgs)
    ./nixidy-envs.nix
  ];
}
