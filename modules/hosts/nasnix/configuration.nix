{ inputs, ... }: {
  flake.modules.nixos.nasnix = { config, inputs, ... }:
    let
      hosts = import ../../../hosts/default.nix { };
      core = [
        {
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
        ../../../hosts/nasnix/base.nix
        ../../../hosts/nasnix/hardware-configuration.nix
        ../../../nixosModules
      ];
      mkSpecialisation = module: {
        inheritParentConfig = false;
        configuration = {
          imports = core ++ [ module ];
          inherit (config) host hosts;
          _module.args = { inherit inputs; };
        };
      };
      specialisations = {
        budgie = mkSpecialisation ../../../environments/budgie;
        hyprland = mkSpecialisation ../../../environments/hyprland;
        gnome = mkSpecialisation ../../../environments/gnome;
        plasma6 = mkSpecialisation ../../../environments/plasma6;
      };
      host-module = {
        inherit hosts;
        host = hosts.nasnix;
        imports = specialisations.budgie.configuration.imports;
        # specialisation = {
        #   inherit (specialisations)
        #   # budgie
        #     gnome hyprland plasma6;
        # };
      };
    in {
      _module.args = { inherit inputs; };
      imports = with inputs.self.modules.nixos; [
        host-module
        # duck
        ../../../nixosModules
      ];
    };
}
