{ ... }:
let
  hosts = import ../../../hosts/default.nix { };
  host = hosts.inspernix;
in {
  flake.modules.homeManager.inspernix = { pkgs, ... }: {
    inherit host hosts;

    home = {
      packages = with pkgs; [ cheese discord nerdfetch ];
      sessionPath = [ "$HOME/.cargo/bin:$PATH" "$HOME/.local/bin:$PATH" ];
    };
  };

  flake.modules.nixos.inspernix = { inputs, pkgs, ... }:
    let
      core = [
        {
          inherit host hosts;

          boot.loader = {
            systemd-boot.enable = true;
            efi.canTouchEfiVariables = true;
          };

          programs = {
            dconf.enable = true;
            firefox.enable = true;

            gnupg.agent = {
              enable = true;
              enableSSHSupport = true;
            };

            nix-ld = {
              enable = true;
              libraries = with pkgs; [ alsa-lib libGL ];
            };
          };

          services = {
            gnome.gnome-keyring.enable = true;
            printing.enable = true;
          };

          time.timeZone = "America/Detroit";
        }
        inputs.self.modules.nixos.base
        inputs.self.modules.nixos.sddm
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
      imports = [ host-module ];
    };
}
