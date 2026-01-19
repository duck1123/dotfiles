{ ... }:
let
  enable-polybar = false;
in
{
  flake.types.generic.feature-options.i3 =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "i3 feature";

  flake.modules.homeManager.i3 =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf config.host.features.i3.enable {
        programs.i3status-rust = {
          enable = true;
        };

        programs.rofi = {
          enable = true;
          # terminal = "${pkgs.alacritty}/bin/alacritty";
          # theme = ./theme.rafi;
        };

        services.polybar = lib.mkIf enable-polybar {
          enable = true;
          config = ../../resources/polybar-config;
          script = ''
            for m in $(polybar --list-monitors | ${pkgs.coreutils}/bin/cut -d":" -f1); do
              MONITOR=$m polybar nord &
            done
          '';
        };

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
    };
}
