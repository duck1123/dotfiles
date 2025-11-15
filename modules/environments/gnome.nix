{ ... }: {
  flake.modules.nixos.environments-gnome = { ... }: {
    services = {
      desktopManager.gnome.enable = true;
      displayManager.defaultSession = "gnome";
    };
  };
}
