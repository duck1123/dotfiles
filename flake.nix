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
    in  {
      # Home configurations
      # Accessible via 'home-manager'
      homeConfigurations = {
        drenfer = homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = {
            inherit inputs;
            identity = config.drenfer;
          };
          modules = [
            stylix.homeManagerModules.stylix
            ./hosts/vavirl-pw0bwnq8/home-for-flake.nix
          ];
        };

        deck = homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = {
            inherit inputs;
            identity = config.deck;
          };
          modules = [
            stylix.homeManagerModules.stylix
            ./hosts/steamdeck/home-for-flake.nix
          ];
        };

        "duck@powerspecnix" = homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = {
            inherit inputs;
            identity = config.duck;
          };
          modules = [
            stylix.homeManagerModules.stylix
            ./hosts/powerspecnix/home-for-flake.nix
          ];
        };
        "duck@inspernix" = homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = {
            inherit inputs;
            identity = config.duck;
          };
          modules = [
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
