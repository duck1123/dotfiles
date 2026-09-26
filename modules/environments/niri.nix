_: {
  flake.modules.nixos.environments-niri =
    { inputs, pkgs, ... }:
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

        # Look in place of fuzzel. niri reads a single config.kdl and has no
        # IPC for adding binds, so the user config has to pull this in with a
        # trailing `include optional=true "/etc/niri/look.kdl"`; binds from a
        # later include override earlier ones, so this wins over the default
        # Mod+D fuzzel bind without editing it out.
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
}
