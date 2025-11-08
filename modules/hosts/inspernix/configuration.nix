{ ... }: {
  flake.modules.home-manager.inspernix = { pkgs, ... }: {
    imports = [ ../../../programs/base ];

    home = {
      packages = with pkgs; [ cheese discord nerdfetch ];
      sessionPath = [ "$HOME/.cargo/bin:$PATH" "$HOME/.local/bin:$PATH" ];
    };
  };

  flake.modules.nixos.inspernix = { config, inputs, ... }:
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
        inputs.self.modules.nixos.base
        ../../../hosts/inspernix/base.nix
        ../../../hosts/inspernix/hardware-configuration.nix
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
      host-module = {
        inherit hosts;
        host = hosts.inspernix;

        imports = specialisations.hyprland.configuration.imports;
        specialisation = {
          inherit (specialisations)
            budgie
            # gnome hyprland i3 plasma6
          ;
        };
      };
    in {
      _module.args = { inherit inputs; };
      imports = [ host-module ../../../nixosModules ];
    };
}
