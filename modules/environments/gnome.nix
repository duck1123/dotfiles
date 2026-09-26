_: {
  environments.gnome.nixos = _: {
    services = {
      desktopManager.gnome.enable = true;
      displayManager.defaultSession = "gnome";
    };
  };
}
