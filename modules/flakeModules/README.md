# Host Validation System

This module provides comprehensive validation for host configurations in your NixOS flake. It ensures that host features are consistent and follows best practices.

## Overview

The validation system consists of three main components:

1. **`validation.nix`** - Core validation functions and feature type definitions
2. **`hostValidator.nix`** - High-level validation functions for multiple hosts
3. **`default.nix`** - Module integration and type definitions

## Features

### Automatic Validation

The system automatically validates:
- **Feature dependencies**: Ensures required features are enabled together
- **Configuration consistency**: Checks NixOS and feature flag alignment
- **Best practices**: Warns about recommended feature combinations

### Validation Rules

#### Desktop Environment Rules
- If `hyprland.enable = true`, recommends enabling `waybar.enable = true`
- If `i3.enable = true`, recommends enabling `waybar.enable = true`
- If `gnome.enable = true`, recommends enabling `dconf.enable = true`

#### Syncthing Rules
- If `syncthing.enable = true`, requires at least one share to be enabled
- Validates share configurations (camera, keepass, org-roam, renpy)

#### Kubernetes Rules
- If `kubernetes.server.enable = true`, requires `kubernetes.client.enable = true`
- If `kubernetes.client.enable = true`, recommends `developer.enable = true`

#### Development Rules
- If `java.enable = true`, recommends `developer.enable = true`
- If `clojure.enable = true`, recommends `developer.enable = true`
- If `dbt.enable = true`, recommends `developer.enable = true`

#### Media Rules
- If `ncmpcpp.enable = true`, requires `music.enable = true`
- If `radio.enable = true`, recommends `media.enable = true`

#### NixOS Consistency Rules
- If NixOS GNOME is enabled, features.gnome should also be enabled
- If NixOS Hyprland is enabled, features.hyprland should also be enabled
- Desktop features should have corresponding NixOS desktop environment

## Usage

### In Your Flake

```nix
{
  # Define your hosts
  hosts = let
    rawHosts = {
      myhost = {
        name = "myhost";
        system = "x86_64-linux";
        # ... other host configuration
        features = {
          hyprland.enable = true;
          waybar.enable = false;  # This will generate a warning
          # ... other features
        };
      };
    };
    
    # Validate all hosts
    validatedHosts = import ./modules/flakeModules/hostValidator.nix { inherit lib; };
    validationResult = validatedHosts.validateHostsComprehensive rawHosts;
    
    # Use validated hosts
    hosts = validationResult.hosts;
  in hosts;
}
```

### Manual Validation

```nix
let
  hostValidator = import ./modules/flakeModules/hostValidator.nix { inherit lib; };
  
  # Validate a single host
  validatedHost = hostValidator.validateHostConfig myHost;
  
  # Validate multiple hosts
  validatedHosts = hostValidator.validateAllHosts myHosts;
  
  # Generate warnings
  warnings = hostValidator.generateAllWarnings myHosts;
  
  # Comprehensive validation
  result = hostValidator.validateHostsComprehensive myHosts;
in {
  # result contains: { hosts, warnings, consistencyReport, isValid }
}
```

### In Home-Manager Modules

```nix
{ config, lib, host, ... }:
{
  # The host is automatically validated when passed to modules
  # You can access validated host configuration
  config = lib.mkIf host.features.hyprland.enable {
    # This will only run if hyprland is enabled
    # And the host has passed validation
  };
}
```

## Error Handling

### Validation Errors
Validation errors will cause the build to fail with descriptive messages:

```
Host myhost: hyprland is enabled but waybar is disabled (recommended to enable waybar with hyprland)
Host myhost: syncthing is enabled but no shares are configured
```

### Warnings
Warnings are displayed but don't stop the build:

```
Host myhost: Consider enabling waybar for better hyprland integration
Host myhost: Consider enabling developer tools for better java development experience
```

## Adding Custom Validation Rules

To add custom validation rules, edit `validation.nix`:

```nix
# Add to validateFeatureDependencies function
customValidation = if host.features.myFeature.enable && !host.features.requiredFeature.enable then
  [ "Host ${host.name}: myFeature requires requiredFeature to be enabled" ]
else [];
```

## Testing

Use the test script to verify validation:

```bash
nix eval -f test-validation.nix validationResults
```

## Best Practices

1. **Enable related features together**: If you enable a desktop environment, enable its supporting tools
2. **Use developer tools**: Enable `developer.enable = true` for development-related features
3. **Configure syncthing shares**: Always configure at least one share when enabling syncthing
4. **Maintain consistency**: Keep NixOS configuration and feature flags aligned
5. **Review warnings**: Address warnings to improve your configuration

## Troubleshooting

### Common Issues

1. **Build fails with validation errors**: Fix the configuration issues mentioned in the error messages
2. **Unexpected warnings**: Review the warning messages and adjust your configuration accordingly
3. **Missing features**: Ensure all required features are defined in your host configuration

### Debug Mode

To see detailed validation information, you can temporarily modify the validation functions to log more details or use the comprehensive validation function to get full reports.
