{ host, lib, ... }: {
  config = lib.mkIf host.features.hyprpanel.enable {
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
            right = [
              "media"
              "network"
              "bluetooth"
              "battery"
              "systray"
              "volume"
              "notifications"
            ];

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

            powermenu = { avatar.name = "Duck"; };

            shortcuts.left.shortcut1 = {
              command = "firefox";
              tooltip = "Firefox";
            };

            stats.enable_gpu = false;
          };
        };

        theme = {
          bar = { buttons = { style = "wave"; }; };

          font = {
            name = "CaskaydiaCove NF";
            size = "12px";
          };
        };
      };
    };
  };
}
