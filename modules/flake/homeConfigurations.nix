{ config, inputs, ... }:
let
  inherit (inputs.home-manager.lib) homeManagerConfiguration;
  system = "x86_64-linux";
  pkgs = import inputs.nixpkgs {
    inherit system;
    config = { allowUnfree = true; };
  };
in {
  flake.homeConfigurations = {
    "drenfer@VAVIRL-PW0BWNQ8" = homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = { inherit inputs; };
      modules = [
        ({ config, ... }: { host = config.hosts.vallenpc; })
        config.flake.modules.home-manager.core
        ../../hosts/vavirl-pw0bwnq8/home.nix
      ];
    };

    "deck@steamdeck" = homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = { inherit inputs; };
      modules = [
        ({ config, ... }: { host = config.hosts.steamdeck; })
        config.flake.modules.home-manager.core
        ../../hosts/steamdeck/home.nix
      ];
    };

    "duck@inspernix" = homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = { inherit inputs; };
      modules = [
        ({ config, ... }: { host = config.hosts.inspernix; })
        config.flake.modules.home-manager.core
        ../../hosts/inspernix/home.nix
      ];
    };

    "duck@nasnix" = homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = { inherit inputs; };
      modules = [
        ({ config, ... }: { host = config.hosts.nasnix; })
        config.flake.modules.home-manager.core
        ../../hosts/nasnix/home.nix
      ];
    };

    "duck@powerspecnix" = homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = { inherit inputs; };
      modules = [
        ({ config, ... }: { host = config.hosts.powerspecnix; })
        config.flake.modules.home-manager.core
        ../../hosts/powerspecnix/home.nix
      ];
    };
  };
}
