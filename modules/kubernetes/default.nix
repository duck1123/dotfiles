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
    "${inputs.k3s-fleetops}/modules/lib/waitForGluetun.nix"
    "${inputs.k3s-fleetops}/modules/applications.nix"
    "${inputs.k3s-fleetops}/modules/sops.nix"
    ./nixidy-envs.nix
  ];
}
