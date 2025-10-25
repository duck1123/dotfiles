{ lib, host, pkgs, ... }: {
  options = {
    features.gaming.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable gaming";
    };
  };

  config = lib.mkIf host.features.gaming.enable {
    home.packages = with pkgs; [
      heroic
      itch
      lutris
      # nexusmods-app
      protontricks
      satisfactorymodmanager
    ];
  };
}
