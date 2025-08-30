{ lib, ... }:
with lib;

let
  # Import validation functions
  validation = import ./validation.nix { inherit lib; };

  # Validate a single host configuration
  validateHostConfig = host: validation.validateHost host;

  # Validate all hosts in a hosts map
  validateAllHosts = hosts:
    builtins.mapAttrs (name: host: validateHostConfig host) hosts;

  # Generate warnings for all hosts
  generateAllWarnings = hosts:
    builtins.concatLists (builtins.attrValues
      (builtins.mapAttrs (name: host: validation.generateWarnings host) hosts));

  # Check for feature consistency across hosts
  checkFeatureConsistency = hosts:
    let
      # Get all enabled features for each host
      hostFeatures = builtins.mapAttrs (name: host: {
        inherit name;
        features = builtins.filter (feature: feature.enable)
          (builtins.attrValues (builtins.mapAttrs (featureName: feature:
            if builtins.isAttrs feature
            && builtins.hasAttr "enable" feature then {
              name = featureName;
              enable = feature.enable;
            } else {
              name = featureName;
              enable = false;
            }) host.features));
      }) hosts;

      # Check for inconsistent feature usage
      inconsistencies = builtins.concatLists (builtins.attrValues
        (builtins.mapAttrs (name: hostInfo:
          builtins.map
          (feature: "Host ${hostInfo.name}: ${feature.name} is enabled")
          hostInfo.features) hostFeatures));
    in inconsistencies;

  # Main validation function that runs all checks
  validateHostsComprehensive = hosts:
    let
      # First validate individual hosts
      validatedHosts = validateAllHosts hosts;

      # Generate warnings
      warnings = generateAllWarnings hosts;

      # Check consistency
      consistencyReport = checkFeatureConsistency hosts;

      # Return validation result
      result = {
        hosts = validatedHosts;
        warnings = warnings;
        consistencyReport = consistencyReport;
        isValid = true;
      };
    in result;

in {
  inherit validateHostConfig validateAllHosts generateAllWarnings
    checkFeatureConsistency validateHostsComprehensive;
}
