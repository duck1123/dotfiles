{ host, lib, pkgs, ... }: {

  options = {
    features.pictures.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable pictures";
    };
  };

  config = lib.mkIf host.features.pictures.enable {
    home.packages = with pkgs; [ digikam gimp ];
  };
}
