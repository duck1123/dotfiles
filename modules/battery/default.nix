{ host, lib, pkgs, ... }: {
  config =
    lib.mkIf host.features.battery.enable { services.upower.enable = true; };
}
