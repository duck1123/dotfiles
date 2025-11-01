{ config, lib, pkgs, ... }: {
  config = lib.mkIf config.host.features.gnome.enable {
    home.packages = with pkgs; [
      gnomeExtensions.appindicator
      gnomeExtensions.gsconnect
      # gnomeExtensions.topicons-plus
      guake
    ];
  };
}

