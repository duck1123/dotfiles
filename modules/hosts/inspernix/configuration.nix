{ ... }:
let
  hosts = import ../../../hosts/default.nix { };
  host = hosts.inspernix;
in {
  flake.modules.homeManager.inspernix = { pkgs, ... }: {
    imports = [ ../../../programs ];
    inherit host hosts;

    home = {
      packages = with pkgs; [ cheese discord nerdfetch ];
      sessionPath = [ "$HOME/.cargo/bin:$PATH" "$HOME/.local/bin:$PATH" ];
    };
  };

  flake.modules.nixos.inspernix = { inputs, ... }:
    let
      core = [
        {
          inherit host hosts;

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
          _module.args = { inherit inputs; };
        };
      };
      specialisations = with inputs.self.modules.nixos; {
        budgie = mkSpecialisation environments-budgie;
        hyprland = mkSpecialisation environments-hyprland;
        gnome = mkSpecialisation environments-gnome;
        i3 = mkSpecialisation environments-i3;
        plasma6 = mkSpecialisation environments-plasma6;
      };
      host-module = {
        imports = specialisations.hyprland.configuration.imports;
        specialisation = {
          inherit (specialisations) budgie;
          # inherit (specialisations) gnome;
          # inherit (specialisations) hyprland;
          # inherit (specialisations) i3;
          # inherit (specialisations) plasma6;
        };
      };
    in {
      _module.args = { inherit inputs; };
      imports = [ host-module ../../../nixosModules ];
    };
}
