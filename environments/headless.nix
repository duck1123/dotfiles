{ lib, ... }:
{
  description = "text-only console environment (no display manager or desktop)";

  # Hosts keep their GUI features (xserver, sddm, sound, bluetooth) for the
  # environments they also enable; this switches their services off while
  # headless is the active one. Packages (fonts, browsers) stay installed but
  # nothing graphical runs. Boot another enabled environment from the boot
  # menu, or switch to it live with
  # /run/current-system/specialisation/<name>/bin/switch-to-configuration switch.
  nixos = _: {
    services = {
      xserver.enable = lib.mkForce false;
      displayManager.sddm.enable = lib.mkForce false;
      pipewire.enable = lib.mkForce false;
    };

    hardware.bluetooth.enable = lib.mkForce false;
    xdg.portal.enable = lib.mkForce false;
    systemd.defaultUnit = lib.mkForce "multi-user.target";
  };
}
