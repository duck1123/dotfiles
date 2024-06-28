{
  description = "home-manager config";

  nixConfig.extra-experimental-features = "nix-command flakes";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-22.05";

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Utilities for building our flake
    flake-utils.url = "github:numtide/flake-utils";

    home = {
      url = "github:nix-community/home-manager/release-22.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      # Use system packages list where available
      inputs.nixpkgs.follows = "nixpkgs";
    };

    deploy = {
      url = "github:serokell/deploy-rs";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Convenience modules for hardware-specific quirks
    hardware.url = "github:nixos/nixos-hardware";

    nixos-generators.url = "github:nix-community/nixos-generators";
    nixos-hardware.url = "github:nixos/nixos-hardware";

    nvfetcher = {
      url = "github:berberman/nvfetcher";
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
        nixos = nixosSystem {
          system = "x86_64-linux";

          modules = [
            home-manager.nixosModules.home-manager
            ./machines/powerspecnix/configuration.nix
          ];
          # Make our inputs available to the config (for importing modules)
          specialArgs = { inherit inputs; };
        };
      };

      packages = eachDefaultSystemMap (system:
        import nixpkgs { inherit system; });

      devShells = eachDefaultSystemMap (system: {
        default = import ./shell.nix { pkgs = packages.${system}; };
      });
    };
}
