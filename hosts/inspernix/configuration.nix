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
  mkSpecialisation = module: {
    inheritParentConfig = false;
    configuration.imports = core ++ [ module ];
  };
  specialisations = {
    hyprland = mkSpecialisation ../../modules/hyprland;
    gnome = mkSpecialisation ../../modules/gnome;
  };
in {
  imports = specialisations.hyprland.configuration.imports;
  specialisation.gnome = specialisations.gnome;
}
