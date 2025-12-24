{ ... }:
let feature-name = "syncthing";
in {
  flake.types.generic.feature-options.${feature-name} = { lib, ... }:
    with lib;
    let
      shareWithPath = mkOption {
        type = types.submodule {
          options = {
            enable = mkOption {
              type = types.bool;
              default = false;
              description = "Enable this share";
            };
            path = mkOption {
              type = types.nullOr types.str;
              default = null;
              description =
                "Path for this share (uses default from shares config if not set)";
            };
          };
        };
        default = { };
        description = "Share configuration";
      };
    in mkOption {
      type = types.submodule {
        options = {
          enable = mkOption {
            type = types.bool;
            default = false;
            description = "Enable syncthing";
          };

          shares = mkOption {
            type = types.submodule {
              options = {
                camera = shareWithPath;
                keepass = shareWithPath;
                org-roam = shareWithPath;
                renpy = shareWithPath;
                sims4 = shareWithPath;
              };
            };
            default = { };
            description = "Syncthing shares configuration";
          };
        };
      };
      default = { };
      description = "Syncthing configuration";
    };

  flake.modules.homeManager.${feature-name} = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.${feature-name}.enable {
      home.packages = with pkgs; [ syncthing ];
    };
  };

  flake.modules.nixos.${feature-name} = { config, lib, ... }:
    with lib; {
      config = mkIf config.host.features.${feature-name}.enable {
        services.syncthing = let
          inherit (config.host.identity) username;
          homeDir = "/home/${username}";
          getDevicesForShare = shareName:
            lists.map (host: host.name) (lists.filter (host:
              (host.features.syncthing.enable or false)
              && (host.features.syncthing.shares.${shareName}.enable or false))
              (attrsets.mapAttrsToList (_: value: value) config.hosts));
          shares = {
            camera = {
              path = "${homeDir}/Camera";
              label = "camera";
            };
            keepass = {
              path = "${homeDir}/keepass";
              label = "keepass";
            };
            org-roam = {
              path = "${homeDir}/org-roam";
              versioning = {
                type = "simple";
                params.keep = "10";
              };
            };
            renpy = {
              path = "${homeDir}/.renpy";
              folderName = "steamdeck-renpy";
            };
            sims4 = {
              path =
                "${homeDir}/.steam/steam/steamapps/compatdata/1222670/pfx/drive_c/users/steamuser/Documents/Electronic Arts/The Sims 4";
            };
          };
        in {
          enable = true;
          user = username;
          dataDir = "${homeDir}/Documents";
          configDir = "${homeDir}/Documents/.config/syncthing";
          openDefaultPorts = true;
          overrideFolders = false;
          overrideDevices = true;

          settings = {
            devices = attrsets.mapAttrs' (name: value: {
              name = value.name;
              value = {
                id = value.id;
                autoAcceptFolders = true;
              };
            }) config.hosts;

            folders = attrsets.mapAttrs' (shareName: shareConfig:
              let
                share = config.host.features.syncthing.shares.${shareName};
                folderName = shareConfig.folderName or shareName;
                # Use host config path if set (not null), otherwise use default from shares config
                path =
                  if share.path != null then share.path else shareConfig.path;
                folderConfig = {
                  inherit path;
                  devices = getDevicesForShare shareName;
                } // optionalAttrs (shareConfig ? label) {
                  label = shareConfig.label;
                } // optionalAttrs (shareConfig ? versioning) {
                  inherit (shareConfig) versioning;
                };
              in attrsets.nameValuePair folderName folderConfig)
              (attrsets.filterAttrs (shareName: _:
                config.host.features.syncthing.shares.${shareName}.enable or false)
                shares);

            options.urAccepted = -1;
          };
        };
      };
    };
}
