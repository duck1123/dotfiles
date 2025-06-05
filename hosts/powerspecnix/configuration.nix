{ inputs, hosts, ... }:
let
  core = [
    { inherit hosts; }
    inputs.disko.nixosModules.disko
    inputs.home-manager.nixosModules.home-manager
    inputs.sops-nix.nixosModules.sops
    inputs.stylix.nixosModules.stylix
    ../../modules/flakeModules
    ./hardware-configuration.nix
    ../../modules/base
    ../../modules/bitcoin
    ../../modules/boot
    ../../modules/i18n
    ../../modules/kubernetes
    ../../modules/nfs
    ../../modules/sddm
    ../../modules/stylix
    ../../modules/syncthing
    ../../modules/users
    # ../../modules/virtualization
  ];
  mkSpecialisation = module: {
    inheritParentConfig = false;
    configuration.imports = core ++ [ module ];
  };
  specialisations = {
    budgie = mkSpecialisation ../../modules/budgie;
    hyprland = mkSpecialisation ../../modules/hyprland;
    gnome = mkSpecialisation ../../modules/gnome;
    i3 = mkSpecialisation ../../modules/i3;
    plasma6 = mkSpecialisation ../../modules/plasma6;
  };
in {
  imports = specialisations.hyprland.configuration.imports;
  specialisation = { inherit (specialisations) budgie gnome i3 plasma6; };
}
