{ lib, ... }:
{
  flake.modules.nixos.environments-plasma6 = _: {
    services.desktopManager.plasma6.enable = true;

    # Force override the Qt platform theme to use "kde" instead of "kde6"
    qt.platformTheme = lib.mkForce "kde";
  };
}
