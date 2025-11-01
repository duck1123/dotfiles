{ config, lib, pkgs, ... }: {
  config = lib.mkIf config.host.features.gaming.enable {
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
