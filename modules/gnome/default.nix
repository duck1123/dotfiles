{ ... }: {
  services = {
    displayManager.defaultSession = "gnome";
    xserver.desktopManager.gnome.enable = true;
  };
}
