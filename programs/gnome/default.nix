{ host, lib, pkgs, ... }: {
  options = {
    features.gnome.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable gnome";
    };
  };

  config = lib.mkIf host.features.gnome.enable {
    home.packages = with pkgs; [
      gnomeExtensions.appindicator
      gnomeExtensions.gsconnect
      # gnomeExtensions.topicons-plus
      guake
    ];
  };
}

