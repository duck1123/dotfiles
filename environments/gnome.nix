{
  features = [ "gnome" ];
  desktopNames = [ "GNOME" ];

  nixos = _: {
    services = {
      desktopManager.gnome.enable = true;
      displayManager.defaultSession = "gnome";
    };
  };
}
