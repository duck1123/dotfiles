{ pkgs, ... }: {
  services = {
    desktopManager.plasma6.enable = true;
    displayManager = {
      sddm = {
        enable = true;
        theme = "ocean";
        wayland.enable = true;
      };
    };
  };
}
