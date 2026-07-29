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
            bar.layout = [
              {
                monitor = "*";

                left = [
                  "dashboard"
                  "hyprland-workspaces"
                  "window-title"
                ];

                center = [ "clock" ];

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
                    "systray"
                    "volume"
                    "notifications"
                  ];
              }
            ];

            modules = {
              clock = {
                format = "%b %d %I:%M:%S %p";
                icon-show = false;
                dropdown-show-seconds = false;
              };

              weather = {
                location = "Detroit";
                units = "metric";
              };

              media.label-show = false;
              network.label-show = false;
            };
          };
        };
      };
    };
}
