_: {
  flake.modules.nixos.environments-budgie = _: {
    services = {
      desktopManager.budgie.enable = true;
      displayManager.defaultSession = "budgie-desktop";
    };
  };
}
