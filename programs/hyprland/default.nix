{ pkgs, ... }: {
  home.packages = with pkgs; [
    cascadia-code
    font-awesome
    grim
    hyprshot
    nautilus
    nerd-fonts.caskaydia-mono
    ncpamixer
    pamixer
    pavucontrol
    socat
    waybar
    wev
    wofi
    wofi-power-menu
  ];

  programs = {
    kitty.enable = true; # required for the default Hyprland config

    rofi.enable = true;

    waybar = {
      enable = true;

      settings = {
        mainBar = {
          layer = "top";
          position = "top";
          modules-left = [ "hyprland/workspaces" ];
          modules-center = [ "clock" ];
          modules-right =
            [ "battery" "tray" "cpu" "memory" "pulseaudio" "network" ];

          cpu.format = "<span color='#b4befe'>🖥️ </span>{usage}%";

          clock = {
            format = "<span color='#b4befe'>🕒 </span>{:%Y-%m-%d  %H:%M}";
            tooltip = true;
            tooltip-format = "{:%Y-%m-%d %a}";
          };

          memory = {
            interval = 1;
            format =
              "<span color='#b4befe'>🧠 </span>{used:0.1f}G/{total:0.1f}G";
          };

          network = {
            interface = "wlp3s0";
            format = "{ifname}";
            format-wifi = "<span color='#b4befe'>🛜 </span>{essid}";
            format-ethernet = "{ipaddr}/{cidr} ";
            format-disconnected = "<span color='#b4befe'> </span>No Network";
            tooltip = false;
          };

          pulseaudio = {
            format = "<span color='#b4befe'>{icon}</span> {volume}%";
            format-muted = "🔇";
            tooltip = false;
            format-icons = {
              headphone = "🎧";
              default = [ "🔊" "🔉" "🔈" ];
            };
            scroll-step = 1;
            on-click = "pavucontrol";
          };

        };
      };
    };

    wofi.enable = true;
  };

  services.dunst = {
    enable = true;

    settings.global = {
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

  wayland.windowManager.hyprland = {
    enable = true;
    settings = {
      "$fileManager" = "nautiulus";
      "$menu" = "wofi --show drun";
      "$mainMod" = "SUPER";
      "$terminal" = "kitty";

      animations = {
        enabled = "yes";
        bezier = "ease, 0.4, 0.02, 0.21, 1";
        animation = [
          "windows, 1, 3.5, ease, slide"
          "windowsOut, 1, 3.5, ease, slide"
          "border, 1, 6, default"
          "fade, 1, 3, ease"
          "workspaces, 1, 3.5, ease"
        ];
      };

      env = [ "XCURSOR_SIZE,24" "HYPRCURSOR_SIZE,24" ];

      exec = [ "pkill waybar & sleep 0.5 && waybar" ];
      "exec-once" = [
        # "hyprctl setcursor Bibata-Modern-Classic 24"
        "dunst"
      ];

      decoration.rounding = 10;

      dwindle = {
        pseudotile = "yes";
        "preserve_split" = "yes";
      };

      general = {
        "gaps_in" = 5;
        "gaps_out" = 10;
        "border_size" = 2;
        layout = "dwindle";
      };

      gestures."workspace_swipe" = false;

      bind = [
        "   $mainMod,                     A, exec, youtube-music"
        "   $mainMod,                     B, exec, firefox"
        "   $mainMod,                     C, killactive,"
        "   $mainMod,                     D, exec, nautilus $(cat ~/.last_dir 2>/dev/null || echo $HOME)"
        # "   $mainMod,                     E, exec, emacs"
        "   $mainMod,                     E, exec, emacsclient -c -a \"\" --eval \"(magit-status \\\"$(cat ~/.last_dir 2>/dev/null || echo $HOME)\\\")\""
        "   $mainMod,                     F, fullscreen,"
        "   $mainMod,                     G, exec, gossip"
        "   $mainMod,                     H, exec, kitty --working-directory \"$(cat ~/.last_dir 2>/dev/null || echo $HOME)\" htop"
        "   $mainMod,                     J, togglesplit,"
        "   $mainMod,                     K, exec, kitty --working-directory \"$(cat ~/.last_dir 2>/dev/null || echo $HOME)\" k9s"
        "   $mainMod,                     L, exec, lens"
        "   $mainMod,                     M, exit,"
        "   $mainMod,                     N, exec, kitty --working-directory \"$(cat ~/.last_dir 2>/dev/null || echo $HOME)\" nu"
        "   $mainMod,                     P, pseudo,"
        "   $mainMod,                     R, exec, rofiWindow"
        "   $mainMod,                     T, exec, teams-for-linux"
        "   $mainMod,                     U, exec, kitty --working-directory \"$(cat ~/.last_dir 2>/dev/null || echo $HOME)\" jjui"
        "   $mainMod,                     V, togglefloating,"
        "   $mainMod,                     w, exec, wofi --show drun"
        "   $mainMod,                RETURN, exec, kitty --working-directory \"$(cat ~/.last_dir 2>/dev/null || echo $HOME)\""
        "   $mainMod,                   Tab, cyclenext,"
        "   $mainMod,                   Tab, bringactivetotop,"
        "           ,                 Print, exec, hyprshot -m region"
        ''SHIFT,                 Print, exec, grim -g "$(slurp)"''
        "           ,      XF86AudioMicMute, exec, pamixer --default-source -t"
        "           , XF86MonBrightnessDown, exec, light -U 20"
        "           ,   XF86MonBrightnessUp, exec, light -A 20"
        "           ,         XF86AudioMute, exec, pamixer -t"
        "           ,  XF86AudioLowerVolume, exec, pamixer -d 5"
        "           ,  XF86AudioRaiseVolume, exec, pamixer -i 5"
        "           ,         XF86AudioPlay, exec, playerctl play-pause"
        "           ,        XF86AudioPause, exec, playerctl play-pause"
        "   $mainMod,                    up, movefocus, u"
        "SUPER_SHIFT,                    up, movewindow, u"
        "   $mainMod,                  down, movefocus, d"
        "SUPER_SHIFT,                  down, movewindow, d"
        "   $mainMod,                  left, movefocus, l"
        "SUPER_SHIFT,                  left, movewindow, l"
        "   $mainMod,                 right, movefocus, r"
        "SUPER_SHIFT,                 right, movewindow, r"
        "   $mainMod,              mouse_up, workspace, e-1"
        "   $mainMod,            mouse_down, workspace, e+1"
      ] ++ (
        # workspaces
        # binds $mainMod + [shift +] {1..9} to [move to] workspace {1..9}
        builtins.concatLists (builtins.genList (i:
          let ws = i + 1;
          in [
            "$mainMod, code:1${toString i}, workspace, ${toString ws}"
            "$mainMod SHIFT, code:1${toString i}, movetoworkspace, ${
              toString ws
            }"
          ]) 9));

      bindm = [
        "$mainMod, mouse:272, movewindow"
        "     ALT, mouse:272, resizewindow"
        "$mainMod, mouse:273, resizewindow"
        "        , mouse:275, movewindow"
        "        , mouse:276, resizewindow"
      ];

      monitor = [ "HDMI-A-1, 1920x1080, 0x0, 1" "DP-3, 1920x1080, 1920x0, 1" ];

      windowrule = [
        "float,class:pavucontrol"
        "float,class:blueman-manager"
        "size 934 525,class:mpv"
        "float,class:mpv"
        "center,class:mpv"
      ];
    };
  };
}
