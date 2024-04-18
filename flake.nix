{
  description = "home-manager config";

  nixConfig.extra-experimental-features = "nix-command flakes";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-22.05";

    home-manager = {
      url = "github:nix-community/home-manager";
      # Use system packages list where available
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Track channels with commits tested and built by hydra
    # nixos.url = "github:nixos/nixpkgs/nixos-22.05";

    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs";

    home.url = "github:nix-community/home-manager/release-22.05";
    home.inputs.nixpkgs.follows = "nixpkgs";

    deploy.url = "github:serokell/deploy-rs";
    deploy.inputs.nixpkgs.follows = "nixpkgs";

    # Utilities for building our flake
    flake-utils.url = "github:numtide/flake-utils";

    # Convenience modules for hardware-specific quirks
    hardware.url = "github:nixos/nixos-hardware";

    nixos-generators.url = "github:nix-community/nixos-generators";
    nixos-hardware.url = "github:nixos/nixos-hardware";

    nvfetcher.url = "github:berberman/nvfetcher";
    nvfetcher.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, home-manager, flake-utils, ... }@inputs:
    let
      # Bring some functions into scope (from builtins and other flakes)
      # inherit (builtins) attrValues;
      inherit (flake-utils.lib) eachSystemMap defaultSystems;
      inherit (nixpkgs.lib) nixosSystem;
      inherit (home-manager.lib) homeManagerConfiguration;
      eachDefaultSystemMap = eachSystemMap defaultSystems;
    in rec {
      # Home configurations
      # Accessible via 'home-manager'
      homeConfigurations = {
        # FIXME: Replace with your username@hostname
        duck = homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux;

          modules =
            [{ imports = [ ./machines/powerspecnix/home-for-flake.nix ]; }];
        };
      };

      # System configurations
      # Accessible via 'nixos-rebuild'
      nixosConfigurations = {
        nixos = nixosSystem {
          system = "x86_64-linux";

          modules = [
            home-manager.nixosModules.home-manager
            # >> Main NixOS configuration file <<
            ./hosts/powerspecnixos/configuration.nix
            # Adds your custom NixOS modules
            # ./modules/nixos
            # Adds overlays
            # { nixpkgs.overlays = attrValues overlays; }
          ];
          # Make our inputs available to the config (for importing modules)
          specialArgs = { inherit inputs; };
        };
      };

      # Packages
      # Accessible via 'nix build'
      packages = eachDefaultSystemMap (system:
        # Propagate nixpkgs' packages, with our overlays applied
        import nixpkgs { inherit system; });

      # Devshell for bootstrapping
      # Accessible via 'nix develop'
      devShells = eachDefaultSystemMap (system: {
        default = import ./shell.nix { pkgs = packages.${system}; };
      });
    };
}
