{ ... }: {
  flake.modules.homeManager.hyprpanel = { config, lib, ... }: {
    config = lib.mkIf config.host.features.hyprpanel.enable {
      # https://github.com/Jas-SinghFSU/HyprPanel/blob/master/nix/module.nix
      programs.hyprpanel = {
        enable = true;

        settings = {
          bar = {
            clock = {
              format = "%b %d  %I:%M:%S %p";
              showIcon = false;
            };
            launcher.autoDetectIcon = true;

            layouts."*" = {
              left = [ "dashboard" "workspaces" "windowtitle" ];
              middle = [ "clock" ];
              right = let
                batteryEnabled =
                  (config.host.features.battery or { enable = false; }).enable;
              in [ "media" "network" "bluetooth" ]
              ++ (lib.optionals batteryEnabled [ "battery" ])
              ++ [ "systray" "volume" "notifications" ];
            };

            media.show_label = false;
            network.label = false;

            workspaces = {
              show_icons = false;
              show_numbered = true;
            };
          };

          menus = {
            clock = {
              time = {
                military = true;
                hideSeconds = true;
              };

              weather = {
                location = "Detroit";
                unit = "metric";
              };
            };

            dashboard = {
              directories.enabled = true;
              powermenu.avatar.name = "Duck";

              shortcuts.left.shortcut1 = {
                command = "firefox";
                tooltip = "Firefox";
              };

              stats.enable_gpu = false;
            };
          };

          theme = {
            bar.buttons.style = "wave";

            font = {
              name = "CaskaydiaCove NF";
              size = "12px";
            };
          };
        };
      };
    };
  };
}

