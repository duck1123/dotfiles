{ host, lib, ... }: {

  options = {
    features.gaming.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable gaming";
    };
  };

  config =
    lib.mkIf host.features.gaming.enable { programs.steam.enable = true; };
}
