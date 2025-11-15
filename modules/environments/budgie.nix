{ ... }: {
  flake.modules.nixos.environments-budgie = { ... }: {
    services = {
      displayManager.defaultSession = "budgie-desktop";
      xserver.desktopManager.budgie.enable = true;
    };
  };
}
