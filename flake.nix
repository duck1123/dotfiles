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

    kubenix.url = "github:hall/kubenix";

    sops-nix.url = "github:Mic92/sops-nix";

    stylix.url = "github:danth/stylix";
  };

  outputs =
    { flake-utils, home-manager, nixpkgs, sops-nix, stylix, ... }@inputs:
    let
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
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      vavirl-pw0bwnq8 = {
        home = import ./machines/vavirl-pw0bwnq8/home-for-flake.nix {
          inherit inputs pkgs;
          config = config.drenfer;
        };
      };
      steamdeck = {
        home = import ./machines/steamdeck/home-for-flake.nix {
          inherit inputs pkgs;
          config = config.deck;
        };
      };
      powerspecnix = {
        home = import ./machines/powerspecnix/home-for-flake.nix {
          inherit inputs pkgs;
          config = config.duck;
        };
        os = import ./machines/powerspecnix/configuration.nix {
          inherit inputs pkgs;
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
        modules = [ powerspecnix.os sops-nix.nixosModules.sops ];
        specialArgs = { inherit inputs; };
        system = "x86_64-linux";
      };

      devShells = eachDefaultSystemMap (system:
        let pkgs = import nixpkgs { inherit system; };
        in {
          default = pkgs.mkShell {
            name = "installation-shell";

            # See https://github.com/disassembler/network/blob/c341a3af27611390f13f86d966767ea30c726a92/shell.nix
            sopsPGPKeyDirs = [ "./nixos/secrets/keys" ];

            buildInputs = with pkgs; [
              age
              babashka
              clojure
              git
              pkgs.home-manager
              keepassxc
              kubectl
              nh
              nix
              nixpkgs-fmt
              runme
              sops
              ssh-to-age
              ssh-to-pgp
              vals
              wget
            ];
          };
        });
    };
}
