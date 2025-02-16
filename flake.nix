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

    hyprland = {
      url = "github:hyprwm/Hyprland";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprspace = {
      url = "github:KZDKM/Hyprspace";
      inputs.hyprland.follows = "hyprland";
    };

    kubenix.url = "github:hall/kubenix";

    stylix.url = "github:danth/stylix";
  };

  outputs = { flake-utils, home-manager, kubenix, nixpkgs, self, stylix, ... }@inputs:
    let
      # inherit (builtins) attrValues;
      inherit (flake-utils.lib) eachSystemMap defaultSystems;
      inherit (nixpkgs.lib) nixosSystem;
      inherit (home-manager.lib) homeManagerConfiguration;
      eachDefaultSystemMap = eachSystemMap defaultSystems;
      config = {
        deck = {
          name = "Duck Nebuchadnezzar";
          username = "deck";
          email = "duck@kronkltd.net";
          gpgKey = "9564904D297DBF3C";
          hostname = "steamdeck";
        };
        drenfer = {
          name = "Daniel E. Renfer";
          username = "drenfer";
          email = "drenfer@vallen.com";
          gpgKey = "9564904D297DBF3C";
          hostname = "vavirl-pw0bwnq8";
        };
        duck = {
          name = "Duck Nebuchadnezzar";
          username = "duck";
          email = "duck@kronkltd.net";
          gpgKey = "9564904D297DBF3C";
          hostname = "powerspecnix";
        };
      };
      vavirl-pw0bwnq8 = {
        home = import ./machines/vavirl-pw0bwnq8/home-for-flake.nix {
          inherit inputs;
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          config = config.drenfer;
        };
      };
      steamdeck = {
        home = import ./machines/steamdeck/home-for-flake.nix {
          inherit inputs;
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          config = config.deck;
        };
      };
      powerspecnix = {
        home = import ./machines/powerspecnix/home-for-flake.nix {
          inherit inputs;
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          config = config.duck;
        };
        os = import ./machines/powerspecnix/configuration.nix {
          inherit inputs;
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          config = config.duck;
        };
      };
    in rec {
      # Home configurations
      # Accessible via 'home-manager'
      homeConfigurations = {
        drenfer = homeManagerConfiguration {
          modules = [ stylix.homeManagerModules.stylix vavirl-pw0bwnq8.home ];
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
        };

        deck = homeManagerConfiguration {
          modules = [ stylix.homeManagerModules.stylix steamdeck.home ];
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
        };

        duck = homeManagerConfiguration {
          modules = [ stylix.homeManagerModules.stylix powerspecnix.home ];
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
        };
      };

      nixosConfigurations.powerspecnix = nixosSystem {
        modules = [ powerspecnix.os ];
        specialArgs = { inherit inputs; };
        system = "x86_64-linux";
      };

      packages =
        eachDefaultSystemMap (system: import nixpkgs { inherit system; });

      devShells = eachDefaultSystemMap (system: {
        default = import ./shell.nix { pkgs = packages.${system}; };
      });
    };
}
