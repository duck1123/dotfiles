{ host, lib, ... }: {
  config = lib.mkIf host.features.i3.enable {
    programs.i3status-rust = { enable = true; };

    programs.rofi = {
      enable = true;
      # terminal = "${pkgs.alacritty}/bin/alacritty";
      # theme = ./theme.rafi;
    };

    # services.polybar = {
    #   enable = true;
    #   config = ./polybar-config;
    #   script = ''
    #     for m in $(polybar --list-monitors | ${pkgs.coreutils}/bin/cut -d":" -f1); do
    #       MONITOR=$m polybar nord &
    #     done
    #   '';
    # };

    xsession.windowManager.i3 = {
      enable = true;
      config = {
        # bars = [ ];
        bars = [
          # {
          #   statusCommand = "i3bar";
          # }
          {
            position = "top";
            statusCommand = "i3status";
          }
        ];

        gaps = {
          inner = 12;
          outer = 5;
          smartBorders = "off";
          smartGaps = true;
        };

        modifier = "Mod4";

        startup = [
          # {
          #   command = "systemctl --user restart polybar";
          #   always = true;
          #   notification = false;
          # }
          {
            command = "systemctl --user start dunst.service";
            always = true;
            notification = false;
          }
        ];

        terminal = "alacritty";
        window.hideEdgeBorders = "smart";
      };
    };
  };
}
