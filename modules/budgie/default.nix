{ pkgs, ... }: {
  services = {
    xserver = {
      desktopManager.budgie.enable = true;
      displayManager.lightdm.enable = true;
    };
    displayManager.defaultSession = "budgie-desktop";
  };
}
