{ config, lib, pkgs, ... }: {
  config = lib.mkIf config.host.features.battery.enable {
    services.upower.enable = true;
  };
}
