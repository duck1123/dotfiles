_: {
  environments.niri = {
    features = [ "wayle" ];
    desktopNames = [ "niri" ];

    nixos =
      {
        config,
        inputs,
        lib,
        pkgs,
        ...
      }:
      let
        kdlStr = s: ''"${lib.escape [ "\\" "\"" ] s}"'';

        # Mirrors the app/media binds from the hyprland feature; window
        # management is left to niri's defaults. Anything run through `sh`
        # goes through `spawn-sh`, the rest through `spawn`.
        lastDir = ''"$(cat ~/.last_dir 2>/dev/null || echo $HOME)"'';
        kitty = "${pkgs.kitty}/bin/kitty";
        inLastDir = cmd: "${kitty} --working-directory ${lastDir} ${cmd}";
        pamixer = "${pkgs.pamixer}/bin/pamixer";
        playerctl = "${pkgs.playerctl}/bin/playerctl";
        brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";
        locked = "allow-when-locked=true";

        binds = [
          {
            key = "Mod+Space";
            props = ''hotkey-overlay-title="Run an Application: Look" allow-inhibiting=false'';
            spawn = [
              "${pkgs.glib.bin}/bin/gdbus"
              "call"
              "--session"
              "--dest"
              "com.look.Desktop"
              "--object-path"
              "/com/look/Desktop"
              "--method"
              "com.look.Desktop.Toggle"
            ];
          }
          {
            key = "Mod+Return";
            props = ''hotkey-overlay-title="Open a Terminal: kitty"'';
            sh = "${kitty} --working-directory ${lastDir}";
          }
          {
            key = "Mod+A";
            spawn = [ "pear-desktop" ];
          }
          {
            key = "Mod+B";
            spawn = [ "zen-beta" ];
          }
          {
            key = "Mod+D";
            sh = "${pkgs.nautilus}/bin/nautilus ${lastDir}";
          }
          {
            key = "Mod+E";
            sh = ''emacsclient -c -a "" --eval "(magit-status \"$(cat ~/.last_dir 2>/dev/null || echo $HOME)\")"'';
          }
          {
            key = "Mod+G";
            spawn = [ "gossip" ];
          }
          {
            key = "Mod+H";
            sh = inLastDir "htop";
          }
          {
            key = "Mod+K";
            sh = inLastDir "k9s";
          }
          {
            key = "Mod+L";
            spawn = [ "lens" ];
          }
          {
            key = "Mod+N";
            sh = inLastDir "pnu";
          }
          {
            key = "Mod+T";
            spawn = [ "teams-for-linux" ];
          }
          {
            key = "Mod+U";
            sh = inLastDir "jjui";
          }
          {
            key = "XF86AudioRaiseVolume";
            props = locked;
            spawn = [
              pamixer
              "-i"
              "5"
            ];
          }
          {
            key = "XF86AudioLowerVolume";
            props = locked;
            spawn = [
              pamixer
              "-d"
              "5"
            ];
          }
          {
            key = "XF86AudioMute";
            props = locked;
            spawn = [
              pamixer
              "-t"
            ];
          }
          {
            key = "XF86AudioMicMute";
            props = locked;
            spawn = [
              pamixer
              "--default-source"
              "-t"
            ];
          }
          {
            key = "XF86AudioPlay";
            props = locked;
            spawn = [
              playerctl
              "play-pause"
            ];
          }
          {
            key = "XF86AudioPause";
            props = locked;
            spawn = [
              playerctl
              "play-pause"
            ];
          }
          {
            key = "XF86MonBrightnessUp";
            props = locked;
            spawn = [
              brightnessctl
              "--class=backlight"
              "set"
              "+20%"
            ];
          }
          {
            key = "XF86MonBrightnessDown";
            props = locked;
            spawn = [
              brightnessctl
              "--class=backlight"
              "set"
              "20%-"
            ];
          }
        ];

        renderBind =
          {
            key,
            props ? "",
            spawn ? null,
            sh ? null,
          }:
          let
            action =
              if sh != null then "spawn-sh ${kdlStr sh}" else "spawn ${lib.concatMapStringsSep " " kdlStr spawn}";
          in
          "    ${key}${lib.optionalString (props != "") " ${props}"} { ${action}; }";
      in
      {
        imports = [ inputs.look.nixosModules.default ];

        environment = {
          sessionVariables.NIXOS_OZONE_WL = "1";

          # niri's stock config binds Mod+T to alacritty and Mod+D to fuzzel;
          # keep both around so it stays usable if binds.kdl isn't loaded.
          # niri spawns xwayland-satellite on demand for X11 apps when it's on
          # PATH.
          systemPackages = with pkgs; [
            alacritty
            fuzzel
            xwayland-satellite
          ];

          etc = {
            # niri reads ~/.config/niri/config.kdl if it exists, else this file.
            # The homeManager body below points the user path back here, so a
            # stale user config can't shadow it.
            "niri/config.kdl".source = pkgs.runCommand "niri-config.kdl" { } ''
              cat ${config.programs.niri.package.src}/resources/default-config.kdl > $out
              printf '\ninclude "/etc/niri/look.kdl"\ninclude "/etc/niri/binds.kdl"\n' >> $out
            '';

            "niri/look.kdl".text = ''
              spawn-at-startup "lookapp"

              // Look floats itself over niri IPC; this only drops the decorations.
              window-rule {
                  match app-id="^lookapp$"
                  open-floating true
                  focus-ring { off; }
                  shadow { off; }
              }
            '';

            # niri has no IPC for adding binds, so these have to be included from
            # config.kdl; binds from a later include override earlier ones, so
            # these win over the stock binds on the same keys (Mod+D fuzzel,
            # Mod+T alacritty, the media keys, ...) without editing them out.
            "niri/binds.kdl".text = ''
              binds {
              ${lib.concatMapStringsSep "\n" renderBind binds}
              }
            '';
          };
        };

        programs = {
          lookapp.enable = true;
          niri.enable = true;
        };

        services.displayManager.defaultSession = "niri";
      };

    # ~/.config is shared by every specialisation, and niri prefers it over
    # /etc/niri/config.kdl, so own it and defer to the system config. /etc/niri
    # only exists while niri is the active environment, which is the only time
    # niri reads this.
    homeManager = _: {
      xdg.configFile."niri/config.kdl".text = ''
        include "/etc/niri/config.kdl"
      '';
    };
  };
}
