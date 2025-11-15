{ ... }:
let
  hostname = "nasnix";
  loadHosts = config: import ../../hosts/default.nix { inherit config; };
in {
  flake.modules = {
    homeManager.${hostname} = { pkgs, config, ... }:
      let
        hosts = loadHosts config;
        host = hosts.${hostname};
      in {
        inherit host hosts;

        home = {
          packages = with pkgs; [ nerdfetch ];
          sessionPath = [ "$HOME/.cargo/bin:$PATH" "$HOME/.local/bin:$PATH" ];
        };
      };

    nixos.${hostname} = { inputs, pkgs, config, ... }:
      let
        hosts = loadHosts config;
        host = hosts.${hostname};
        core-module = {
          inherit host hosts;

          boot.loader.grub = {
            enable = true;
            device = "/dev/vda";
            useOSProber = true;
          };

          environment.systemPackages = with pkgs; [ samba ];

          services.samba = {
            enable = true;
            settings.global = {
              security = "user";
              "client min protocol" = "SMB2";
              "client max protocol" = "SMB3";
              workgroup = "WORKGROUP";
            };
          };

          system.stateVersion = "25.05";
          time.timeZone = "America/Detroit";
        };
        core = [
          core-module
          inputs.self.modules.nixos.base
          ../../hosts/nasnix/hardware-configuration.nix
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
          plasma6 = mkSpecialisation environments-plasma6;
        };
      in {
        _module.args = { inherit inputs; };
        imports = specialisations.budgie.configuration.imports;
        specialisation = {
          # inherit (specialisations) budgie;
          # inherit (specialisations) gnome;
          # inherit (specialisations) hyprland;
          # inherit (specialisations) plasma6;
        };
      };
  };
}
