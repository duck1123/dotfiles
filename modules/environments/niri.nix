_: {
  environments.niri = {
    features = [ "wayle" ];
    desktopNames = [ "niri" ];

    nixos =
      { inputs, pkgs, ... }:
      {
        imports = [ inputs.look.nixosModules.default ];

        environment = {
          sessionVariables.NIXOS_OZONE_WL = "1";

          # niri's stock config binds Mod+T to alacritty and Mod+D to fuzzel;
          # keep both around so it stays usable if home-manager hasn't written
          # its config yet.
          # niri spawns xwayland-satellite on demand for X11 apps when it's on
          # PATH.
          systemPackages = with pkgs; [
            alacritty
            fuzzel
            xwayland-satellite
          ];
        };

        programs = {
          lookapp.enable = true;
          niri = {
            enable = true;
            # Lets the back side button drag windows like Mod+LMB, which niri
            # hardcodes and can't bind from config.
            package = pkgs.niri.overrideAttrs (old: {
              patches = (old.patches or [ ]) ++ [ ./niri-side-button-drag.patch ];
            });
          };
        };

        services.displayManager.defaultSession = "niri";
      };

    # All of niri's config lives in ~/.config/niri, so a home-manager switch is
    # enough to change it; niri reloads it on its own. niri prefers this path
    # over /etc/niri/config.kdl, and would otherwise write its stock config
    # here on first start.
    homeManager =
      { lib, pkgs, ... }:
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
            spawn = [ "plexamp" ];
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
            key = "Mod+V";
            spawn = [ "code" ];
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
        xdg.configFile = {
          # The stock config with our files included last; includes resolve
          # relative to this file.
          "niri/config.kdl".source = pkgs.runCommand "niri-config.kdl" { } ''
            cat ${pkgs.niri.src}/resources/default-config.kdl > $out
            printf '\ninclude "input.kdl"\ninclude "look.kdl"\ninclude "binds.kdl"\n' >> $out
          '';

          # Like Hyprland's follow_mouse, but only for windows already fully on
          # screen, so moving the mouse never scrolls the layout.
          "niri/input.kdl".text = ''
            input {
                focus-follows-mouse max-scroll-amount="0%"
            }
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
  };
}
