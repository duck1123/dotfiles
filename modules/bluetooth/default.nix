{ host, lib, ... }: {
  options = {
    features.bluetooth.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable bluetooth";
    };
  };

  config = lib.mkIf host.features.bluetooth.enable {
    hardware.bluetooth.enable = true;
  };
}
