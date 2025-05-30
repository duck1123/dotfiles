{ ... }: {
  # https://github.com/Jas-SinghFSU/HyprPanel/blob/master/nix/module.nix
  programs.hyprpanel = {
    enable = true;
    hyprland.enable = true;
    overwrite.enable = true;

    settings = {
      bar = {
        clock = {
          format = "%b %d  %I:%M:%S %p";
          showIcon = false;
        };
        launcher.autoDetectIcon = true;
        media.show_label = false;
        network.label = false;
        workspaces = {
          show_icons = false;
          show_numbered = true;
        };
      };

      layout."bar.layouts" = let
        config = {
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
      in {
        "0" = config;
        "1" = config;
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

          powermenu = {
            avatar.name = "Duck";
          };

          shortcuts.left.shortcut1 = {
            command = "firefox";
            tooltip = "Firefox";
          };

          stats.enable_gpu = false;
        };
      };

      theme = {
        bar = {
          buttons = {
            style = "wave";
          };
        };

        font = {
          name = "CaskaydiaCove NF";
          size = "12px";
        };
      };
    };
  };
}
