{ host, lib, pkgs, ... }: {
  config = lib.mkIf host.features.pictures.enable {
    home.packages = with pkgs; [ digikam gimp ];
  };
}
