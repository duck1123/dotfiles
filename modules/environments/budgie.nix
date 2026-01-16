{ ... }: {
  flake.modules.nixos.environments-budgie = { ... }: {
    services = {
      desktopManager.budgie.enable = true;
      displayManager.defaultSession = "budgie-desktop";
    };
  };
}
