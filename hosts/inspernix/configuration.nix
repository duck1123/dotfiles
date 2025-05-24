{ inputs, pkgs, ... }:
let
  core = [
    ./hardware-configuration.nix
    ../../modules/inspernix-base
    ../../modules/boot
    ../../modules/i18n
    ../../modules/sddm
    ../../modules/stylix
    ../../modules/syncthing
    inputs.stylix.nixosModules.stylix
  ];
in {
  imports = core ++ [ ../../modules/hyprland ];

  environment.systemPackages = with pkgs; [
    libgtop
    wl-clipboard
  ];

  hardware.bluetooth.enable = true;

  services = {
    power-profiles-daemon.enable = true;
    upower.enable = true;
  };

  specialisation.gnome = {
    inheritParentConfig = false;
    configuration.imports = core ++ [ ../../modules/gnome ];
  };
}
