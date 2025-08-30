{ lib, host, pkgs, ... }: {
  config = lib.mkIf host.features.gaming.enable {
    home.packages = with pkgs; [
      heroic
      itch
      lutris
      # nexusmods-app
      protontricks
      # satisfactorymodmanager
    ];
  };
}
