{ host, lib, pkgs, ... }: {
  options = {
    features.flipper.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable flipper";
    };
  };

  config = lib.mkIf host.features.flipper.enable {
    home.packages = with pkgs; [ qFlipper ];
  };
}
