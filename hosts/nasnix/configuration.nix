{ inputs, hosts, ... }:
let
  core = [
    {
      inherit hosts;

      boot.loader.grub = {
        enable = true;
        device = "/dev/vda";
        useOSProber = true;
      };

      nixpkgs.overlays = [ inputs.sddm-sugar-candy-nix.overlays.default ];
    }
    inputs.home-manager.nixosModules.home-manager
    inputs.sddm-sugar-candy-nix.nixosModules.default
    inputs.sops-nix.nixosModules.sops
    inputs.stylix.nixosModules.stylix
    ./base.nix
    ./hardware-configuration.nix
    ../../nixosModules
  ];
  mkSpecialisation = module: {
    inheritParentConfig = false;
    configuration.imports = core ++ [ module ];
  };
  specialisations = {
    budgie = mkSpecialisation ../../environments/budgie;
    hyprland = mkSpecialisation ../../environments/hyprland;
    gnome = mkSpecialisation ../../environments/gnome;
    plasma6 = mkSpecialisation ../../environments/plasma6;
  };
in {
  imports = specialisations.budgie.configuration.imports;
  # specialisation = {
  #   inherit (specialisations)
  #   # budgie
  #     gnome hyprland plasma6;
  # };
}
