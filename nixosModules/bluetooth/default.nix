{ config, lib, pkgs, ... }: {
  config = lib.mkIf config.host.features.bluetooth.enable {
    hardware.bluetooth.enable = true;
  };
}
