{ pkgs, ... }: {
  services = {
    displayManager = {
      defaultSession = "gnome";
      sddm = {
        enable = true;
        theme = "ocean";
        wayland.enable = true;
      };
    };
    xserver.desktopManager.gnome.enable = true;
  };
}
