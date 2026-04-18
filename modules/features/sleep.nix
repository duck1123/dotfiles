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
        services.logind = {
          lidSwitch = "ignore";
          lidSwitchDocked = "ignore";
          extraConfig = "HandleSuspendKey=ignore";
        };

        systemd.sleep.extraConfig = ''
          AllowSuspend=no
          AllowHibernation=no
          AllowHybridSleep=no
          AllowSuspendThenHibernate=no
        '';
      };
    };
}
