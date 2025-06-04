{ inputs, ... }:
let
  core = [
    inputs.disko.nixosModules.disko
    inputs.home-manager.nixosModules.home-manager
    inputs.sops-nix.nixosModules.sops
    inputs.stylix.nixosModules.stylix
    ./hardware-configuration.nix
    ../../modules/inspernix-base
    ../../modules/boot
    ../../modules/i18n
    ../../modules/sddm
    ../../modules/stylix
    ../../modules/syncthing
  ];
in {
  imports = core ++ [ ../../modules/hyprland ];

  specialisation.gnome = {
    inheritParentConfig = false;
    configuration.imports = core ++ [ ../../modules/gnome ];
  };
}
