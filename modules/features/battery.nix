{ ... }: {
  flake.modules.nixos.battery-feature = { config, lib, ... }: {
    config = lib.mkIf config.host.features.battery.enable {
      services.upower.enable = true;
    };
  };
}
