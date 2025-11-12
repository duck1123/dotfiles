{ config, lib, pkgs, ... }: {
  config = lib.mkIf config.host.features.hyprland.enable {
    home.packages = with pkgs; [
      cascadia-code
      font-awesome
      grim
      hyprpanel
      hyprshot
      nautilus
      ncpamixer
      nwg-dock-hyprland
      nwg-drawer
      nwg-launchers
      pamixer
      pavucontrol
      socat
      wev
      wofi
    ];

    programs.kitty.enable = true;

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
        exec = [

          # "hyprpanel"
          # "waybar"
        ];
        decoration.rounding = 10;

        dwindle = {
          pseudotile = "yes";
          "preserve_split" = "yes";
        };

        general = {
          "gaps_in" = 5;
          "gaps_out" = 5;
          "border_size" = 2;
          layout = "dwindle";
        };

        # gestures."workspace_swipe" = false;

        bind = (map (x: "${x.mod},${x.key},${x.command},${x.arg}") [
          # Letter key bindings (sorted by key)
          {
            mod = "$mainMod";
            key = "a";
            command = "exec";
            arg = "youtube-music";
          }
          {
            mod = "$mainMod";
            key = "b";
            command = "exec";
            arg = "zen-beta";
          }
          {
            mod = "$mainMod";
            key = "c";
            command = "killactive";
            arg = "";
          }
          {
            mod = "$mainMod";
            key = "d";
            command = "exec";
            arg = ''nautilus "$(cat ~/.last_dir 2>/dev/null || echo $HOME)"'';
          }
          {
            mod = "$mainMod";
            key = "e";
            command = "exec";
            arg = ''
              emacsclient -c -a "" --eval "(magit-status \"$(cat ~/.last_dir 2>/dev/null || echo $HOME)\")"'';
          }
          {
            mod = "$mainMod";
            key = "f";
            command = "fullscreen";
            arg = "";
          }
          {
            mod = "$mainMod";
            key = "g";
            command = "exec";
            arg = "gossip";
          }
          {
            mod = "$mainMod";
            key = "h";
            command = "exec";
            arg = ''
              kitty --working-directory "$(cat ~/.last_dir 2>/dev/null || echo $HOME)" htop'';
          }
          {
            mod = "$mainMod";
            key = "j";
            command = "togglesplit";
            arg = "";
          }
          {
            mod = "$mainMod";
            key = "k";
            command = "exec";
            arg = ''
              kitty --working-directory "$(cat ~/.last_dir 2>/dev/null || echo $HOME)" k9s'';
          }
          {
            mod = "$mainMod";
            key = "l";
            command = "exec";
            arg = "lens";
          }
          {
            mod = "$mainMod";
            key = "m";
            command = "exit";
            arg = "";
          }
          {
            mod = "$mainMod";
            key = "n";
            command = "exec";
            arg = ''
              kitty --working-directory "$(cat ~/.last_dir 2>/dev/null || echo $HOME)" nu'';
          }
          {
            mod = "$mainMod";
            key = "p";
            command = "pseudo";
            arg = "";
          }
          {
            mod = "$mainMod";
            key = "r";
            command = "exec";
            arg = "rofiWindow";
          }
          {
            mod = "$mainMod";
            key = "t";
            command = "exec";
            arg = "teams-for-linux";
          }
          {
            mod = "$mainMod";
            key = "u";
            command = "exec";
            arg = ''
              kitty --working-directory "$(cat ~/.last_dir 2>/dev/null || echo $HOME)" jjui'';
          }
          {
            mod = "$mainMod";
            key = "v";
            command = "togglefloating";
            arg = "";
          }
          {
            mod = "$mainMod";
            key = "w";
            command = "exec";
            arg = "nwg-drawer";
          }
          # Direction key bindings
          {
            mod = "$mainMod";
            key = "down";
            command = "movefocus";
            arg = "d";
          }
          {
            mod = "SUPER_SHIFT";
            key = "down";
            command = "movewindow";
            arg = "d";
          }
          {
            mod = "$mainMod";
            key = "left";
            command = "movefocus";
            arg = "l";
          }
          {
            mod = "SUPER_SHIFT";
            key = "left";
            command = "movewindow";
            arg = "l";
          }
          {
            mod = "$mainMod";
            key = "right";
            command = "movefocus";
            arg = "r";
          }
          {
            mod = "SUPER_SHIFT";
            key = "right";
            command = "movewindow";
            arg = "r";
          }
          {
            mod = "$mainMod";
            key = "up";
            command = "movefocus";
            arg = "u";
          }
          {
            mod = "SUPER_SHIFT";
            key = "up";
            command = "movewindow";
            arg = "u";
          }
          # Special key bindings
          {
            mod = "$mainMod";
            key = "mouse_down";
            command = "workspace";
            arg = "e+1";
          }
          {
            mod = "$mainMod";
            key = "mouse_up";
            command = "workspace";
            arg = "e-1";
          }
          {
            mod = "";
            key = "Print";
            command = "exec";
            arg = "hyprshot -m region";
          }
          {
            mod = "SHIFT";
            key = "Print";
            command = "exec";
            arg = ''grim -g "$(slurp)"'';
          }
          {
            mod = "$mainMod";
            key = "RETURN";
            command = "exec";
            arg = ''
              kitty --working-directory "$(cat ~/.last_dir 2>/dev/null || echo $HOME)"'';
          }
          {
            mod = "$mainMod";
            key = "SPACE";
            command = "exec";
            arg = "nwg-drawer";
          }
          {
            mod = "$mainMod";
            key = "Tab";
            command = "cyclenext";
            arg = "";
          }
          {
            mod = "$mainMod";
            key = "Tab";
            command = "bringactivetotop";
            arg = "";
          }
          # Media keys
          {
            mod = "";
            key = "XF86AudioLowerVolume";
            command = "exec";
            arg = "pamixer -d 5";
          }
          {
            mod = "";
            key = "XF86AudioMicMute";
            command = "exec";
            arg = "pamixer --default-source -t";
          }
          {
            mod = "";
            key = "XF86AudioMute";
            command = "exec";
            arg = "pamixer -t";
          }
          {
            mod = "";
            key = "XF86AudioPause";
            command = "exec";
            arg = "playerctl play-pause";
          }
          {
            mod = "";
            key = "XF86AudioPlay";
            command = "exec";
            arg = "playerctl play-pause";
          }
          {
            mod = "";
            key = "XF86AudioRaiseVolume";
            command = "exec";
            arg = "pamixer -i 5";
          }
          {
            mod = "";
            key = "XF86MonBrightnessDown";
            command = "exec";
            arg = "light -U 20";
          }
          {
            mod = "";
            key = "XF86MonBrightnessUp";
            command = "exec";
            arg = "light -A 20";
          }
        ]) ++ (
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

        bindm = (map (x: "${x.mod},${x.key},${x.command}") [
          {
            mod = "$mainMod";
            key = "mouse:272";
            command = "movewindow";
          }
          {
            mod = "ALT";
            key = "mouse:272";
            command = "resizewindow";
          }
          {
            mod = "$mainMod";
            key = "mouse:273";
            command = "resizewindow";
          }
          {
            mod = "";
            key = "mouse:275";
            command = "movewindow";
          }
          {
            mod = "";
            key = "mouse:276";
            command = "resizewindow";
          }
        ]);

        # FIXME: This is environment specific
        monitor =
          [ "HDMI-A-1, 1920x1080, 0x0, 1" "DP-3, 1920x1080, 1920x0, 1" ];

        windowrule = [
          "float,class:pavucontrol"
          "float,class:blueman-manager"
          "size 934 525,class:mpv"
          "float,class:mpv"
          "center,class:mpv"
        ];
      };
    };
  };
}
