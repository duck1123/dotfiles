{ inputs, hosts, ... }:
let
  core = [
    {
      inherit hosts;
      boot.efiBoot = true;

      nixpkgs.overlays = [ ];
    }
    inputs.home-manager.nixosModules.home-manager
    inputs.sddm-sugar-candy-nix.nixosModules.default
    inputs.sops-nix.nixosModules.sops
    inputs.stylix.nixosModules.stylix
    ./base.nix
    ./hardware-configuration.nix
    ../../modules
  ];
  mkSpecialisation = module: {
    inheritParentConfig = false;
    configuration.imports = core ++ [ module ];
  };
  specialisations = {
    budgie = mkSpecialisation ../../environments/budgie;
    hyprland = mkSpecialisation ../../environments/hyprland;
    gnome = mkSpecialisation ../../environments/gnome;
    i3 = mkSpecialisation ../../environments/i3;
    plasma6 = mkSpecialisation ../../environments/plasma6;
  };
in {
  imports = specialisations.hyprland.configuration.imports;
  specialisation = {
    inherit (specialisations)
      budgie
      # gnome i3 hyprland plasma6
    ;
  };
}
