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

    k3s-fleetops = {
      url = "github:duck1123/k3s-fleetops";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    kubenix = {
      url = "github:hall/kubenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix = {
      url = "github:danth/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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
        inspernix = {
          name = "Duck Nebuchadnezzar";
          username = "duck";
          email = "duck@kronkltd.net";
          gpgKey = "9564904D297DBF3C";
          hostname = "inspernix";
        };
      };
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        # May the FOSS gods take mercy upon me
        config.allowUnfree = true;
      };

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
      inspernix = {
        home = import ./machines/inspernix/home-for-flake.nix {
          inherit inputs pkgs;
          config = config.inspernix;
        };
        os = import ./machines/inspernix/configuration.nix {
          inherit inputs pkgs;
          config = config.inspernix;
        };
      };
      machines = { inherit inspernix powerspecnix steamdeck vavirl-pw0bwnq8; };
    in rec {
      inherit machines;

      # Home configurations
      # Accessible via 'home-manager'
      homeConfigurations = {
        drenfer = homeManagerConfiguration {
          inherit pkgs;
          modules = [ stylix.homeManagerModules.stylix vavirl-pw0bwnq8.home ];
        };

        deck = homeManagerConfiguration {
          inherit pkgs;
          modules = [ stylix.homeManagerModules.stylix steamdeck.home ];
        };

        "duck@powerspecnix" = homeManagerConfiguration {
          inherit pkgs;
          modules = [ stylix.homeManagerModules.stylix powerspecnix.home ];
        };
        "duck@inspernix" = homeManagerConfiguration {
          inherit pkgs;
          modules = [ stylix.homeManagerModules.stylix inspernix.home ];
        };
      };

      nixosConfigurations = {
        inspernix = nixosSystem {
          modules = [ inspernix.os sops-nix.nixosModules.sops ];
          specialArgs = { inherit inputs; };
          system = "x86_64-linux";
        };
        powerspecnix = nixosSystem {
          modules = [ powerspecnix.os sops-nix.nixosModules.sops ];
          specialArgs = { inherit inputs; };
          system = "x86_64-linux";
        };
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
