{ ... }: {
  flake.modules.nixos.powerspecnix = { config, inputs, ... }:
    let
      hosts = import ../../../hosts/default.nix { };
      core = [
        {
          boot.loader = {
            systemd-boot.enable = true;
            efi.canTouchEfiVariables = true;
          };

          nixpkgs.overlays = [ inputs.sddm-sugar-candy-nix.overlays.default ];
        }
        inputs.home-manager.nixosModules.home-manager
        inputs.sddm-sugar-candy-nix.nixosModules.default
        inputs.sops-nix.nixosModules.sops
        inputs.stylix.nixosModules.stylix
        ../../../hosts/powerspecnix/base.nix
        ../../../hosts/powerspecnix/hardware-configuration.nix
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
        i3 = mkSpecialisation ../../../environments/i3;
        plasma6 = mkSpecialisation ../../../environments/plasma6;
      };
    in {
      # Provide module arguments that modules need
      _module.args = { inherit inputs; };
      imports = with inputs.self.modules.nixos; [
        {
          inherit hosts;
          host = hosts.powerspecnix;
          imports = specialisations.hyprland.configuration.imports;
          specialisation = {
            inherit (specialisations)
              budgie
              # gnome i3 hyprland plasma6
            ;
          };
        }
        # duck
        ../../../nixosModules
      ];
    };
}
