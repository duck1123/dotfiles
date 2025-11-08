{ ... }: {
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
        inputs.self.modules.nixos.base
        ../../../hosts/nasnix/base.nix
        ../../../hosts/nasnix/hardware-configuration.nix
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
      imports = [ host-module ../../../nixosModules ];
    };
}
