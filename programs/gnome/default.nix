{ host, lib, pkgs, ... }: {
  config = lib.mkIf host.features.gnome.enable {
    home.packages = with pkgs; [
      gnomeExtensions.appindicator
      gnomeExtensions.gsconnect
      # gnomeExtensions.topicons-plus
      guake
    ];
  };
}

