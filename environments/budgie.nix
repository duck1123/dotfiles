{
  desktopNames = [ "Budgie" ];

  nixos = _: {
    services = {
      desktopManager.budgie.enable = true;
      displayManager.defaultSession = "budgie-desktop";
    };
  };
}
