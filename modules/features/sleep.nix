{ ... }:
{
  flake.types.generic.feature-options.sleep =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "sleep feature";

  flake.modules.nixos.sleep-feature =
    { config, lib, ... }:
    {
      config = lib.mkIf config.host.features.sleep.enable {
        services.logind.settings.Login = {
          HandleLidSwitch = "ignore";
          HandleLidSwitchDocked = "ignore";
          HandleSuspendKey = "ignore";
        };

        systemd.sleep.settings.Sleep = {
          AllowSuspend = false;
          AllowHibernation = false;
          AllowHybridSleep = false;
          AllowSuspendThenHibernate = false;
        };
      };
    };
}
