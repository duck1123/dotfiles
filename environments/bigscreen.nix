{ lib, ... }:
{
  description = "KDE Plasma Bigscreen (TV / couch) environment";
  desktopNames = [ "KDE" ];

  # nixpkgs packages Plasma Bigscreen but has no module for it: it runs on top
  # of Plasma 6, so enable that and register the package's Wayland session.
  nixos =
    { pkgs, ... }:
    let
      bigscreen = pkgs.kdePackages.plasma-bigscreen;
    in
    {
      environment.systemPackages = [ bigscreen ];

      services = {
        desktopManager.plasma6.enable = true;

        displayManager = {
          sessionPackages = [ bigscreen ];
          defaultSession = "plasma-bigscreen-wayland";
        };
      };

      # Same override as the plasma6 environment
      qt.platformTheme = lib.mkForce "kde";
    };
}
