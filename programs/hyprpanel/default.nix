{ ... }: {
  # https://github.com/Jas-SinghFSU/HyprPanel/blob/master/nix/module.nix
  programs.hyprpanel = {
    enable = true;
    hyprland.enable = true;
    overwrite.enable = true;
    # Override the final config with an arbitrary set.
    # Useful for overriding colors in your selected theme.
    # Default: {}
    override.theme.bar.menus.text = "#123ABC";

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

      theme = {
        bar.transparent = true;

        font = {
          name = "CaskaydiaCove NF";
          size = "16px";
        };
      };
    };
  };
}
