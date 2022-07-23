{
  description = "home-manager config";

  nixConfig.extra-experimental-features = "nix-command flakes";
  # nixConfig.extra-substituters =
  #  "https://nrdxp.cachix.org https://nix-community.cachix.org";
  # nixConfig.extra-trusted-public-keys =
  #  "nrdxp.cachix.org-1:Fc5PSqY2Jm1TrWfm88l6cvGWwz3s93c6IOifQWnhNW4= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-21.11";

    home-manager = {
      url = "github:nix-community/home-manager";
      # Use system packages list where available
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # nix-colors.url = "github:misterio77/nix-colors";   # Color schemes for usage with home-manager
    # impermanence.url = "github:riscadoa/impermanence"; # Utilities for opt-in persistance
    # TODO: Add any other flakes you need

    # Track channels with commits tested and built by hydra
    nixos.url = "github:nixos/nixpkgs/nixos-22.05";
    latest.url = "github:nixos/nixpkgs/nixos-unstable";
    # For darwin hosts: it can be helpful to track this darwin-specific stable
    # channel equivalent to the `nixos-*` channels for NixOS. For one, these
    # channels are more likely to provide cached binaries for darwin systems.
    # But, perhaps even more usefully, it provides a place for adding
    # darwin-specific overlays and packages which could otherwise cause build
    # failures on Linux systems.
    nixpkgs-darwin-stable.url = "github:NixOS/nixpkgs/nixpkgs-22.05-darwin";

    # agenix.url = "github:ryantm/agenix";
    # agenix.inputs.nixpkgs.follows = "nixos";

    # digga.url = "github:divnix/digga";
    # digga.inputs.nixpkgs.follows = "nixos";
    # digga.inputs.nixlib.follows = "nixos";
    # digga.inputs.home-manager.follows = "home";
    # digga.inputs.deploy.follows = "deploy";

    home.url = "github:nix-community/home-manager/release-22.05";
    home.inputs.nixpkgs.follows = "nixos";

    # darwin.url = "github:LnL7/nix-darwin";
    # darwin.inputs.nixpkgs.follows = "nixpkgs-darwin-stable";

    deploy.url = "github:serokell/deploy-rs";
    deploy.inputs.nixpkgs.follows = "nixos";

    # Utilities for building our flake
    flake-utils.url = "github:numtide/flake-utils";

    # Extra flakes for modules, packages, etc

    # Convenience modules for hardware-specific quirks
    hardware.url = "github:nixos/nixos-hardware";

    # naersk.url = "github:nmattia/naersk";
    # naersk.inputs.nixpkgs.follows = "nixos";

    nixos-generators.url = "github:nix-community/nixos-generators";
    nixos-hardware.url = "github:nixos/nixos-hardware";

    # nur.url = "github:nix-community/NUR";              # User contributed pkgs and modules

    nvfetcher.url = "github:berberman/nvfetcher";
    nvfetcher.inputs.nixpkgs.follows = "nixos";

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
      # TODO: If you want to use packages exported from other flakes, add their overlays here.
      # They will be added to your 'pkgs'
      # overlays = {
      #   default = import ./overlay; # Our own overlay
      #   # nur = nur.overlay
      # };

      # System configurations
      # Accessible via 'nixos-rebuild'
      nixosConfigurations = {
        # FIXME: Replace with your hostname
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

      # Home configurations
      # Accessible via 'home-manager'
      homeConfigurations = {
        # FIXME: Replace with your username@hostname
        duck = homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          # inherit pkgs;

          modules = [{
            imports = [ ./machines/powerspecnix/home-for-flake.nix ];

            home = {
              username = "duck";
              homeDirectory = "/home/duck";
              stateVersion = "21.11";
            };
            nix.registry.nixpkgs.flake = nixpkgs;
          }];
          # FIXME: Replace with your username
          # username = "duck";
          # homeDirectory = "/home/${username}";
          # system = "x86_64-linux";

          # # >> Main home-manager configuration file <<
          # configuration = ./home-manager/home.nix;
          # extraModules = [
          #   # Adds your custom home-manager modules
          #   ./modules/home-manager
          #   # Adds overlays
          #   # { nixpkgs.overlays = attrValues overlays; }
          # ];
          # # Make our inputs available to the config (for importing modules)
          # extraSpecialArgs = { inherit inputs; };
        };
      };

      # Packages
      # Accessible via 'nix build'
      packages = eachDefaultSystemMap (system:
        # Propagate nixpkgs' packages, with our overlays applied
        import nixpkgs {
          inherit system;
          # overlays = attrValues overlays;
        });

      # Devshell for bootstrapping
      # Accessible via 'nix develop'
      devShells = eachDefaultSystemMap (system: {
        default = import ./shell.nix { pkgs = packages.${system}; };
      });
    };
}
