{ config, lib, ... }:
with lib;
let
  # Import validation functions
  validation = import ./validation.nix { inherit lib; };
  hostValidator = import ./hostValidator.nix { inherit lib; };
  
  # Define the feature types that can be enabled/disabled
  featureType = types.submodule {
    options = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether this feature is enabled";
      };
    };
  };

  # Define the kubernetes feature type specifically
  kubernetesType = types.submodule {
    options = {
      client.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether kubernetes client is enabled";
      };
      server.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether kubernetes server is enabled";
      };
    };
  };

  # Define the syncthing shares type
  syncthingSharesType = types.submodule {
    options = {
      camera.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether camera syncthing share is enabled";
      };
      keepass.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether keepass syncthing share is enabled";
      };
      org-roam.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether org-roam syncthing share is enabled";
      };
      renpy.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether renpy syncthing share is enabled";
      };
    };
  };

  # Define the syncthing feature type
  syncthingType = types.submodule {
    options = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether syncthing is enabled";
      };
      shares = mkOption {
        type = syncthingSharesType;
        default = {};
        description = "Syncthing shares configuration";
      };
    };
  };

  # Define the nixos feature type
  nixosType = types.submodule {
    options = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether NixOS is enabled";
      };
      budgie.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether Budgie desktop is enabled";
      };
      gnome.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether GNOME desktop is enabled";
      };
      hyprland.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether Hyprland is enabled";
      };
      i3.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether i3 is enabled";
      };
      plasma6.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether Plasma 6 is enabled";
      };
    };
  };

  # Define the complete host type with all features
  hostType = types.submodule {
    options = {
      system = mkOption {
        type = types.str;
        description = "The system architecture";
      };
      id = mkOption {
        type = types.str;
        description = "The unique host identifier";
      };
      identity = mkOption {
        type = types.attrs;
        description = "The user identity configuration";
      };
      name = mkOption {
        type = types.str;
        description = "The human-readable host name";
      };
      hostname = mkOption {
        type = types.str;
        description = "The system hostname";
      };
      android = mkOption {
        type = types.submodule {
          options = {
            enable = mkOption {
              type = types.bool;
              default = false;
              description = "Whether Android features are enabled";
            };
          };
        };
        default = {};
        description = "Android-specific configuration";
      };
      features = mkOption {
        type = types.submodule {
          options = {
            backups = featureType;
            bitcoin = featureType;
            clojure = featureType;
            dbt = featureType;
            dconf = featureType;
            developer = featureType;
            dunst = featureType;
            emacs = featureType;
            emacs-prelude = featureType;
            email = featureType;
            flipper = featureType;
            gaming = featureType;
            git = featureType;
            gnome = featureType;
            hyprland = featureType;
            hyprpanel = featureType;
            i3 = featureType;
            java = featureType;
            jujutsu = featureType;
            kubernetes = kubernetesType;
            media = featureType;
            music = featureType;
            ncmpcpp = featureType;
            nfs = featureType;
            nostr = featureType;
            nushell = featureType;
            office = featureType;
            pictures = featureType;
            radio = featureType;
            stylix = featureType;
            syncthing = syncthingType;
            vim = featureType;
            virtualization = featureType;
            vscode = featureType;
            waybar = featureType;
            zsh = featureType;
          };
        };
        description = "Feature flags for the host";
      };
      nixos = mkOption {
        type = nixosType;
        default = {};
        description = "NixOS-specific configuration";
      };
      home-manager = mkOption {
        type = types.submodule {
          options = {
            enable = mkOption {
              type = types.bool;
              default = false;
              description = "Whether home-manager is enabled";
            };
          };
        };
        default = {};
        description = "Home-manager configuration";
      };
    };
  };

in {
  options = {
    host = mkOption {
      type = hostType;
      description = "The host configuration";
      default = {};
      apply = validation.validateHost;
    };

    hosts = mkOption {
      type = types.attrsOf hostType;
      description = "Map of hosts with their configurations";
      default = {};
      apply = validation.validateHosts;
    };
  };

  config = {
    # Add validation warnings to the system
    warnings = let
      host = config.host;
    in validation.generateWarnings host;
  };


}

