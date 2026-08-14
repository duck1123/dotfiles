{ lib, ... }:
{
  flake.modules.homeManager.environments-gnome =
    { config, ... }:
    {
      gtk.gtk4.theme = lib.mkDefault config.gtk.theme;
    };

  flake.modules.nixos.environments-gnome = _: {
    services = {
      desktopManager.gnome.enable = true;
      displayManager.defaultSession = "gnome";
    };
  };
}
