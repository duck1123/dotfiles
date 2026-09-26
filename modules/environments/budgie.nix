_: {
  # Not enabled on any host: nixpkgs' budgie module references pkgs.qogir-theme,
  # which was removed upstream (depended on gtk-engine-murrine/GTK2)
  environments.budgie.nixos = _: {
    services = {
      desktopManager.budgie.enable = true;
      displayManager.defaultSession = "budgie-desktop";
    };
  };
}
