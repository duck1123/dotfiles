{ inputs, ... }:
{
  imports = [
    # Pull in all lib functions (loadSecrets, fromYAML, mkArgoApp, etc.),
    # the nixidyApps option, all application modules, and the ageRecipients module.
    # Use path-based imports so this works without k3s-fleetops declaring a flakeModule.
    "${inputs.k3s-fleetops}/modules/flake/flake-parts.nix"
    "${inputs.k3s-fleetops}/modules/lib/fromYAML.nix"
    "${inputs.k3s-fleetops}/modules/lib/toYAML.nix"
    "${inputs.k3s-fleetops}/modules/lib/loadSecrets.nix"
    "${inputs.k3s-fleetops}/modules/lib/mkArgoApp.nix"
    "${inputs.k3s-fleetops}/modules/lib/mkPinnedVolume.nix"
    "${inputs.k3s-fleetops}/modules/lib/waitForGluetun.nix"
    "${inputs.k3s-fleetops}/modules/applications.nix"
    "${inputs.k3s-fleetops}/modules/sops.nix"
    # modules/pkgs declares flake.packages some apps reference directly
    # (e.g. self.packages.<system>.duck1123-runtime) -- import-tree'd rather
    # than listed file-by-file since fleetops adds new ones over time and a
    # missing one fails evaluation deep inside an unrelated app's build.
    (inputs.import-tree "${inputs.k3s-fleetops}/modules/pkgs")
    ./nixidy-envs.nix
  ];
}
