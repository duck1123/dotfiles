_: {
  environments.niri = {
    features = [ "wayle" ];
    desktopNames = [ "niri" ];

    nixos =
      {
        config,
        inputs,
        pkgs,
        ...
      }:
      {
        imports = [ inputs.look.nixosModules.default ];

        environment = {
          sessionVariables.NIXOS_OZONE_WL = "1";

          # niri's generated default config binds Mod+T to alacritty, and spawns
          # xwayland-satellite on demand for X11 apps when it's on PATH.
          systemPackages = with pkgs; [
            alacritty
            xwayland-satellite
          ];

          # niri reads ~/.config/niri/config.kdl if it exists, else this file, and
          # only writes its stock config to the user path when neither exists. So
          # shipping the stock config plus the Look include here covers fresh
          # hosts; an existing user config still wins and needs the same
          # trailing include added by hand.
          etc."niri/config.kdl".source = pkgs.runCommand "niri-config.kdl" { } ''
            cat ${config.programs.niri.package.src}/resources/default-config.kdl > $out
            printf '\ninclude "/etc/niri/look.kdl"\n' >> $out
          '';

          # Look in place of fuzzel. niri has no IPC for adding binds, so this
          # has to be included from config.kdl; binds from a later include
          # override earlier ones, so this wins over the default Mod+D fuzzel
          # bind without editing it out.
          etc."niri/look.kdl".text = ''
            spawn-at-startup "lookapp"

            binds {
                Mod+D hotkey-overlay-title="Run an Application: Look" allow-inhibiting=false { spawn "${pkgs.glib.bin}/bin/gdbus" "call" "--session" "--dest" "com.look.Desktop" "--object-path" "/com/look/Desktop" "--method" "com.look.Desktop.Toggle"; }
            }

            // Look floats itself over niri IPC; this only drops the decorations.
            window-rule {
                match app-id="^lookapp$"
                open-floating true
                focus-ring { off; }
                shadow { off; }
            }
          '';
        };

        programs = {
          lookapp.enable = true;
          niri.enable = true;
        };

        services.displayManager.defaultSession = "niri";
      };
  };
}
