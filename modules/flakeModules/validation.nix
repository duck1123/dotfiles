{ lib, ... }:
with lib;

let
  # Validation functions for different feature combinations
  validateFeatureDependencies = host:
    let
      errors = [ ];

      # Desktop environment validations
      desktopErrors =
        # If hyprland features are enabled, recommend waybar
        (if host.features.hyprland.enable && !host.features.waybar.enable then
          [
            "Host ${host.name}: hyprland is enabled but waybar is disabled (recommended to enable waybar with hyprland)"
          ]
        else
          [ ]) ++

        # If i3 features are enabled, recommend waybar
        (if host.features.i3.enable && !host.features.waybar.enable then
          [
            "Host ${host.name}: i3 is enabled but waybar is disabled (recommended to enable waybar with i3)"
          ]
        else
          [ ]) ++

        # If gnome features are enabled, recommend dconf
        (if host.features.gnome.enable && !host.features.dconf.enable then
          [
            "Host ${host.name}: gnome is enabled but dconf is disabled (recommended to enable dconf with gnome)"
          ]
        else
          [ ]);

      # Syncthing validations
      syncthingErrors = if host.features.syncthing.enable then
        let
          shares = host.features.syncthing.shares;
          hasShares = shares.camera.enable || shares.keepass.enable
            || shares.org-roam.enable || shares.renpy.enable;
        in if !hasShares then
          [
            "Host ${host.name}: syncthing is enabled but no shares are configured"
          ]
        else
          [ ]
      else
        [ ];

      # Kubernetes validations
      kubernetesErrors =
        # Server requires client
        (if host.features.kubernetes.server.enable
        && !host.features.kubernetes.client.enable then
          [
            "Host ${host.name}: kubernetes server is enabled but client is disabled (server requires client)"
          ]
        else
          [ ]) ++

        # Client should have kubectl access
        (if host.features.kubernetes.client.enable
        && !host.features.developer.enable then
          [
            "Host ${host.name}: kubernetes client is enabled but developer tools are disabled (recommended for kubectl access)"
          ]
        else
          [ ]);

      # Development environment validations
      devErrors =
        # Java development
        (if host.features.java.enable && !host.features.developer.enable then
          [
            "Host ${host.name}: java is enabled but developer tools are disabled (recommended for java development)"
          ]
        else
          [ ]) ++

        # Clojure development
        (if host.features.clojure.enable && !host.features.developer.enable then
          [
            "Host ${host.name}: clojure is enabled but developer tools are disabled (recommended for clojure development)"
          ]
        else
          [ ]) ++

        # DBT development
        (if host.features.dbt.enable && !host.features.developer.enable then
          [
            "Host ${host.name}: dbt is enabled but developer tools are disabled (recommended for dbt development)"
          ]
        else
          [ ]);

      # Media environment validations
      mediaErrors =
        # Music player requires music features
        (if host.features.ncmpcpp.enable && !host.features.music.enable then
          [
            "Host ${host.name}: ncmpcpp is enabled but music features are disabled (ncmpcpp requires music features)"
          ]
        else
          [ ]) ++

        # Radio requires radio features
        (if host.features.radio.enable && !host.features.media.enable then
          [
            "Host ${host.name}: radio is enabled but media features are disabled (recommended to enable media features with radio)"
          ]
        else
          [ ]);

      # NixOS configuration validations
      nixosErrors = if host.nixos.enable then
      # Desktop environment consistency
        (if host.nixos.gnome.enable && !host.features.gnome.enable then
          [
            "Host ${host.name}: NixOS GNOME is enabled but gnome features are disabled (inconsistent configuration)"
          ]
        else
          [ ]) ++

        (if host.nixos.hyprland.enable && !host.features.hyprland.enable then
          [
            "Host ${host.name}: NixOS Hyprland is enabled but hyprland features are disabled (inconsistent configuration)"
          ]
        else
          [ ]) ++

        (if host.nixos.plasma6.enable && !host.features.gnome.enable then
          [
            "Host ${host.name}: NixOS Plasma 6 is enabled but gnome features are disabled (recommended for consistent desktop experience)"
          ]
        else
          [ ]) ++

        # Ensure at least one desktop environment is selected if features suggest it
        (let
          hasDesktopFeatures = host.features.hyprland.enable
            || host.features.gnome.enable || host.features.i3.enable;
          hasDesktopNixos = host.nixos.hyprland.enable
            || host.nixos.gnome.enable || host.nixos.plasma6.enable
            || host.nixos.budgie.enable || host.nixos.i3.enable;
        in if hasDesktopFeatures && !hasDesktopNixos then
          [
            "Host ${host.name}: desktop features are enabled but no desktop environment is selected in NixOS configuration"
          ]
        else
          [ ])
      else
        [ ];

      allErrors = errors ++ desktopErrors ++ syncthingErrors ++ kubernetesErrors
        ++ devErrors ++ mediaErrors ++ nixosErrors;
    in if allErrors != [ ] then
      throw ''
        Host validation errors:
      '' + builtins.concatStringsSep "\n" allErrors
    else
      host;

  # Generate warnings for potential issues
  generateWarnings = host:
    let
      warnings = [ ];

      # Desktop environment warnings
      desktopWarnings =
        # Hyprland without waybar
        (if host.features.hyprland.enable && !host.features.waybar.enable then
          [
            "Host ${host.name}: Consider enabling waybar for better hyprland integration"
          ]
        else
          [ ]) ++

        # Stylix without desktop environment
        (if host.features.stylix.enable && host.nixos.enable then
          let
            hasDesktop = host.nixos.hyprland.enable || host.nixos.gnome.enable
              || host.nixos.plasma6.enable || host.nixos.budgie.enable
              || host.nixos.i3.enable;
          in if !hasDesktop then
            [
              "Host ${host.name}: stylix is enabled but no desktop environment is selected in NixOS configuration"
            ]
          else
            [ ]
        else
          [ ]);

      # Development environment warnings
      devWarnings =
        # Java without developer tools
        (if host.features.java.enable && !host.features.developer.enable then
          [
            "Host ${host.name}: Consider enabling developer tools for better java development experience"
          ]
        else
          [ ]) ++

        # Clojure without developer tools
        (if host.features.clojure.enable && !host.features.developer.enable then
          [
            "Host ${host.name}: Consider enabling developer tools for better clojure development experience"
          ]
        else
          [ ]);

      # Media environment warnings
      mediaWarnings =
        # Gaming without media features
        (if host.features.gaming.enable && !host.features.media.enable then
          [
            "Host ${host.name}: Consider enabling media features for better gaming experience"
          ]
        else
          [ ]) ++

        # Pictures without media features
        (if host.features.pictures.enable && !host.features.media.enable then
          [
            "Host ${host.name}: Consider enabling media features for better picture management"
          ]
        else
          [ ]);

      allWarnings = warnings ++ desktopWarnings ++ devWarnings ++ mediaWarnings;
    in allWarnings;

  # Main validation function
  validateHost = host: validateFeatureDependencies host;

  # Validation function for multiple hosts
  validateHosts = hosts:
    builtins.mapAttrs (name: host: validateHost host) hosts;

in { inherit validateHost validateHosts generateWarnings; }
