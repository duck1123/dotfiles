{ host, lib, pkgs, ... }: {
  config = lib.mkIf host.features.flipper.enable {
    home.packages = with pkgs; [ qFlipper ];
  };
}
