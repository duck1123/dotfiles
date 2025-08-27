{ inputs, hosts, ... }:
let
  core = [
    { inherit hosts; }
    inputs.disko.nixosModules.disko
    inputs.home-manager.nixosModules.home-manager
    inputs.sops-nix.nixosModules.sops
    inputs.stylix.nixosModules.stylix
    ./hardware-configuration.nix
    ../../modules/bitcoin
    ../../modules/boot
    ../../modules/flakeModules
    ./base.nix
    ../../modules/i18n
    ../../modules/network
    # ../../modules/sddm
    ../../modules/stylix
    ../../modules/syncthing
  ];
  mkSpecialisation = module: {
    inheritParentConfig = false;
    configuration.imports = core ++ [ module ];
  };
  specialisations = {
    budgie = mkSpecialisation ../../modules/budgie;
    hyprland = mkSpecialisation ../../modules/hyprland;
    gnome = mkSpecialisation ../../modules/gnome;
    plasma6 = mkSpecialisation ../../modules/plasma6;
  };
in {
  imports = specialisations.budgie.configuration.imports;
  specialisation.gnome = specialisations.gnome;
  specialisation.budgie = specialisations.budgie;
  # specialisation.hyprland = specialisations.hyprland;
  # specialisation.plasma6 = specialisations.plasma6;
}
