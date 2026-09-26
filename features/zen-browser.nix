{
  homeManager =
    { config, lib, ... }:
    {
      programs.zen-browser = {
        enable = true;
        policies = {
          DisableAppUpdate = true;
          DisableTelemetry = true;
        };
      };

      stylix.targets.zen-browser.profileNames = lib.mkIf config.host.features.stylix.enable [ "default" ];
    };
}
