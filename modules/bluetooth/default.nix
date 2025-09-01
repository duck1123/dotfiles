{ host, lib, pkgs, ... }: {
  config = lib.mkIf host.features.bluetooth.enable {
    hardware.bluetooth.enable = true;
  };
}
