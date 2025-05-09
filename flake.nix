{
  description = "home-manager config";

  nixConfig.extra-experimental-features = "nix-command flakes";

  inputs = {
    clj-nix = {
      url = "github:jlesquembre/clj-nix";
      inputs.nix-fetcher-data.follows = "nix-fetcher-data";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-compat.url = "github:edolstra/flake-compat";

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs-lib";
    };

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      # Use system packages list where available
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprland = {
      url = "github:hyprwm/Hyprland";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprpanel = {
      url = "github:jas-singhfsu/hyprpanel";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    k3s-fleetops = {
      url = "github:duck1123/k3s-fleetops";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.sops-nix.follows = "sops-nix";
    };

    kubenix = {
      url = "github:hall/kubenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    make-shell = {
      url = "github:nicknovitski/make-shell";
      inputs.flake-compat.follows = "flake-compat";
    };

    mkdocs-flake = {
      url = "github:applicative-systems/mkdocs-flake";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.poetry2nix.follows = "poetry2nix";
    };

    nix-fetcher-data = {
      url = "github:jlesquembre/nix-fetcher-data";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-kube-generators.url = "github:farcaller/nix-kube-generators";

    nixhelm = {
      url = "github:farcaller/nixhelm";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nix-kube-generators.follows = "nix-kube-generators";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.poetry2nix.follows = "poetry2nix";
    };

    nixidy = {
      url = "github:duck1123/nixidy?ref=feature/chmod";
      inputs.flake-utils.follows = "flake-utils";
      inputs.kubenix.follows = "kubenix";
      inputs.nix-kube-generators.follows = "nix-kube-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    nixpkgs-lib.url = "github:nix-community/nixpkgs.lib";

    poetry2nix = {
      url = "github:nix-community/poetry2nix";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.systems.follows = "systems";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix = {
      url = "github:danth/stylix";
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-utils.follows = "flake-utils";
      inputs.home-manager.follows = "home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.systems.follows = "systems";
    };

    systems.url = "github:nix-systems/default";
  };

  outputs = { flake-utils, home-manager, hyprpanel, nixpkgs, sops-nix, stylix
    , ... }@inputs:
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
        overlays = [ inputs.hyprpanel.overlay ];
      };
    in {
      # Home configurations
      # Accessible via 'home-manager'
      homeConfigurations = {
        drenfer = homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = {
            inherit inputs system;
            identity = config.drenfer;
          };
          modules = [
            hyprpanel.homeManagerModules.hyprpanel
            stylix.homeManagerModules.stylix
            ./hosts/vavirl-pw0bwnq8/home-for-flake.nix
          ];
        };

        deck = homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = {
            inherit inputs system;
            identity = config.deck;
          };
          modules = [
            hyprpanel.homeManagerModules.hyprpanel
            stylix.homeManagerModules.stylix
            ./hosts/steamdeck/home-for-flake.nix
          ];
        };

        "duck@powerspecnix" = homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = {
            inherit inputs system;
            identity = config.duck;
          };
          modules = [
            hyprpanel.homeManagerModules.hyprpanel
            stylix.homeManagerModules.stylix
            ./hosts/powerspecnix/home-for-flake.nix
          ];
        };
        "duck@inspernix" = homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = {
            inherit inputs system;
            identity = config.duck;
          };
          modules = [
            hyprpanel.homeManagerModules.hyprpanel
            stylix.homeManagerModules.stylix
            ./hosts/inspernix/home-for-flake.nix
          ];
        };
      };

      nixosConfigurations = {
        inspernix = nixosSystem {
          modules =
            [ ./hosts/inspernix/configuration.nix sops-nix.nixosModules.sops ];
          specialArgs = {
            inherit inputs;
            identity = config.duck;
          };
          system = "x86_64-linux";
        };
        powerspecnix = nixosSystem {
          modules = [
            ./hosts/powerspecnix/configuration.nix
            sops-nix.nixosModules.sops
          ];
          specialArgs = {
            inherit inputs;
            identity = config.duck;
          };
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
