{
  programs.i3status-rust = { enable = true; };

  programs.rofi = {
    enable = true;
    # terminal = "${pkgs.alacritty}/bin/alacritty";
    # theme = ./theme.rafi;
  };

  services.dunst = {
    enable = true;
    # iconTheme = {
    #   name = "Adwaita";
    #   package = pkgs.gnome3.adwaita-icon-theme;
    #   size = "16x16";
    # };

    settings = {
      global = {
        monitor = 0;
        geometry = "600x50-50+65";
        shrink = "yes";
        transparency = 10;
        padding = 16;
        horizontal_padding = 16;
        # font = "JetBrainsMono Nerd Font 10";
        line_height = 4;
        format = "<b>%s</b>\\n%b";
      };
    };
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

      window = { hideEdgeBorders = "smart"; };
    };
  };

}
