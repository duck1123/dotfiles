{
  description = "home-manager config";

  nixConfig.extra-experimental-features = "nix-command flakes";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    # Utilities for building our flake
    flake-utils.url = "github:numtide/flake-utils";

    home-manager = {
      url = "github:nix-community/home-manager";
      # Use system packages list where available
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, flake-utils, ... }@inputs:
    let
      # inherit (builtins) attrValues;
      inherit (flake-utils.lib) eachSystemMap defaultSystems;
      inherit (nixpkgs.lib) nixosSystem;
      inherit (home-manager.lib) homeManagerConfiguration;
      eachDefaultSystemMap = eachSystemMap defaultSystems;
    in rec {
      # Home configurations
      # Accessible via 'home-manager'
      homeConfigurations = {
        drenfer = homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux;

          modules =
            [{ imports = [ ./machines/vavirl-pw0bwnq8/home-for-flake.nix ]; }];
        };

        deck = homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux;

          modules =
            [{ imports = [ ./machines/steamdeck/home-for-flake.nix ]; }];
        };

        duck = homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux;

          modules =
            [{ imports = [ ./machines/powerspecnix/home-for-flake.nix ]; }];
        };
      };

      nixosConfigurations = {
        powerspecnix = nixosSystem {
          system = "x86_64-linux";

          modules = [
            home-manager.nixosModules.home-manager
            ./machines/powerspecnix/configuration.nix
          ];
          # Make our inputs available to the config (for importing modules)
          specialArgs = { inherit inputs; };
        };
      };

      packages =
        eachDefaultSystemMap (system: import nixpkgs { inherit system; });

      devShells = eachDefaultSystemMap (system: {
        default = import ./shell.nix { pkgs = packages.${system}; };
      });
    };
}
