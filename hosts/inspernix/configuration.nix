{ inputs, ... }:
let
  core = [
    ./hardware-configuration.nix
    ../../modules/inspernix-base
    ../../modules/boot
    ../../modules/i18n
    ../../modules/sddm
    ../../modules/stylix
    ../../modules/syncthing
    inputs.home-manager.nixosModules.home-manager
    inputs.stylix.nixosModules.stylix
  ];
in {
  imports = core ++ [ ../../modules/hyprland ];
  hardware.bluetooth.enable = true;
  services.upower.enable = true;

  specialisation.gnome = {
    inheritParentConfig = false;
    configuration.imports = core ++ [ ../../modules/gnome ];
  };
}
