{ inputs, ... }:
let pkgs = inputs.pkgs;
in {
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
