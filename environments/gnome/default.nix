{ ... }: {
  services = {
    desktopManager.gnome.enable = true;
    displayManager.defaultSession = "gnome";
  };
}
