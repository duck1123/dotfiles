{ host, lib, ... }: {

  options = {
    features.battery.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable battery";
    };
  };

  config =
    lib.mkIf host.features.battery.enable { services.upower.enable = true; };
}
