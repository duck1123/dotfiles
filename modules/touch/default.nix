{ host, lib, ... }: {
  options = {
    features.touch.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable touch";
    };
  };

  config =
    lib.mkIf host.features.touch.enable { services.libinput.enable = true; };
}
