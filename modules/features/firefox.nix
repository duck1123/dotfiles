{ ... }:
{
  flake.types.generic.feature-options.firefox =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "firefox feature";

  flake.modules.homeManager.firefox =
    { config, lib, ... }:
    {
      config = lib.mkIf config.host.features.firefox.enable {
        programs.firefox = {
          configPath = "${config.xdg.configHome}/mozilla/firefox";
          enable = true;
        };
      };
    };

  flake.modules.nixos.firefox-feature =
    { config, lib, pkgs, ... }:
    {
      config = lib.mkIf config.host.features.firefox.enable {
        users.users."${config.host.identity.username}".packages = [ pkgs.firefox ];
      };
    };
}
