{ ... }:
{
  flake.modules.homeManager.environments-gnome =
    { config, ... }:
    {
      gtk.gtk4.theme = config.gtk.theme;
    };

  flake.modules.nixos.environments-gnome =
    { ... }:
    {
      services = {
        desktopManager.gnome.enable = true;
        displayManager.defaultSession = "gnome";
      };
    };
}
