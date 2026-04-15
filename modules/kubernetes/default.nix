{ inputs, ... }:
{
  imports = [
    # Pull in all lib functions (loadSecrets, fromYAML, mkArgoApp, etc.),
    # the nixidyApps option, all application modules, and the ageRecipients module.
    inputs.k3s-fleetops.flakeModules.default
    ./nixidy-envs.nix
  ];
}
