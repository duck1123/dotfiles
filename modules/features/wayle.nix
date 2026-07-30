{ ... }:
{
  flake.types.generic.feature-options.wayle =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "wayle feature";

  flake.modules.homeManager.wayle =
    { config, lib, ... }:
    {
      config = lib.mkIf config.host.features.wayle.enable {
        # https://github.com/nix-community/home-manager/blob/master/modules/services/wayle.nix
        services.wayle = {
          enable = true;

          settings = {
            # font + palette are managed by stylix's wayle target
            bar = {
              bg = "transparent";
              border-location = "bottom";
              button-icon-padding = 0.5;
              button-icon-size = 0.8;
              button-label-size = 0.8;

              layout = [
                {
                  monitor = "*";

                  left = [
                    "dashboard"
                    "hyprland-workspaces"
                    "window-title"
                  ];

                  center = [
                    "clock"
                    "weather"
                    "notifications"
                  ];

                  right =
                    let
                      batteryEnabled = (config.host.features.battery or { enable = false; }).enable;
                    in
                    [
                      "media"
                      "network"
                      "bluetooth"
                    ]
                    ++ (lib.optionals batteryEnabled [ "battery" ])
                    ++ [
                      "netstat"
                      "microphone"
                      "volume"
                      "systray"
                    ];
                }
              ];
            };

            modules = {
              bluetooth.label-show = false;

              clock = {
                format = "%b %d %I:%M:%S %p";
                icon-show = false;
                dropdown-show-seconds = false;
              };

              dashboard.icon-bg-color = "bg-overlay";
              media.label-show = false;
              microphone.label-show = false;
              network.label-show = false;
              volume.label-show = false;

              weather = {
                location = "Detroit";
                units = "metric";
              };
            };
          };
        };
      };
    };
}
