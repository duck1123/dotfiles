{ ... }: {
  # https://github.com/Jas-SinghFSU/HyprPanel/blob/master/nix/module.nix
  programs.hyprpanel = {
    enable = true;
    hyprland.enable = true;
    overwrite.enable = true;

    settings = {
      bar = {
        launcher.autoDetectIcon = true;
        workspaces.show_icons = true;
      };

      layout."bar.layouts" = let
        config = {
          left = [ "dashboard" "workspaces" "windowtitle" ];
          middle = [ "clock" ];
          right = [
            "volume"
            "media"
            "systray"
            "battery"
            "network"
            "bluetooth"
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
          weather.unit = "metric";
        };

        dashboard = {
          directories.enabled = false;

          shortcuts.left.shortcut1 = {
            command = "firefox";
            tooltip = "Firefox";
          };

          stats.enable_gpu = true;
        };
      };

      theme.font = {
        name = "CaskaydiaCove NF";
        size = "12px";
      };
    };
  };
}
