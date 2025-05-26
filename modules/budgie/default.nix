{ pkgs, ... }: {
  services = {
    displayManager.defaultSession = "budgie-desktop";
    xserver.desktopManager.budgie.enable = true;
  };
}
