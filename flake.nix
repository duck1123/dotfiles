{
  description = "home-manager config";

  nixConfig.extra-experimental-features = "nix-command flakes";

  inputs = {
    clj-nix = {
      inputs = {
        devshell.follows = "devshell";
        nix-fetcher-data.follows = "nix-fetcher-data";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:jlesquembre/clj-nix";
    };

    colmena = {
      inputs = {
        flake-compat.follows = "flake-compat";
        flake-utils.follows = "flake-utils";
        nix-github-actions.follows = "nix-github-actions";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:zhaofengli/colmena";
    };

    devshell = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:numtide/devshell";
    };

    disko = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/disko";
    };

    flake-compat.url = "github:edolstra/flake-compat";

    flake-parts = {
      inputs.nixpkgs-lib.follows = "nixpkgs-lib";
      url = "github:hercules-ci/flake-parts";
    };

    flake-utils = {
      inputs.systems.follows = "systems";
      url = "github:numtide/flake-utils";
    };

    gitignore = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:hercules-ci/gitignore.nix";
    };

    haumea = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/haumea";
    };

    home-manager = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/home-manager";
    };

    hyprland = {
      inputs = {
        nixpkgs.follows = "nixpkgs";
        pre-commit-hooks.follows = "pre-commit-hooks";
        systems.follows = "systems";
      };
      url = "github:hyprwm/Hyprland";
    };

    hyprpanel = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:jas-singhfsu/hyprpanel";
    };

    k3s-fleetops = {
      inputs = {
        clj-nix.follows = "clj-nix";
        flake-compat.follows = "flake-compat";
        flake-parts.follows = "flake-parts";
        flake-utils.follows = "flake-utils";
        kubenix.follows = "kubenix";
        make-shell.follows = "make-shell";
        mkdocs-flake.follows = "mkdocs-flake";
        nix-fetcher-data.follows = "nix-fetcher-data";
        nix-kube-generators.follows = "nix-kube-generators";
        nixidy.follows = "nixidy";
        nixhelm.follows = "nixhelm";
        nixpkgs.follows = "nixpkgs";
        nixpkgs-lib.follows = "nixpkgs-lib";
        poetry2nix.follows = "poetry2nix";
        sops-nix.follows = "sops-nix";
        systems.follows = "systems";
      };
      url = "github:duck1123/k3s-fleetops";
    };

    kubenix = {
      inputs = {
        flake-compat.follows = "flake-compat";
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
        treefmt.follows = "treefmt-nix";
      };
      url = "github:hall/kubenix";
    };

    make-shell = {
      inputs.flake-compat.follows = "flake-compat";
      url = "github:nicknovitski/make-shell";
    };

    mkdocs-flake = {
      inputs = {
        flake-parts.follows = "flake-parts";
        nixpkgs.follows = "nixpkgs";
        poetry2nix.follows = "poetry2nix";
      };
      url = "github:applicative-systems/mkdocs-flake";
    };

    nix-fetcher-data = {
      inputs = {
        flake-parts.follows = "flake-parts";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:jlesquembre/nix-fetcher-data";
    };

    nix-github-actions = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/nix-github-actions";
    };

    nix-kube-generators.url = "github:farcaller/nix-kube-generators";

    nixhelm = {
      inputs = {
        flake-utils.follows = "flake-utils";
        haumea.follows = "haumea";
        nix-kube-generators.follows = "nix-kube-generators";
        nixpkgs.follows = "nixpkgs";
        poetry2nix.follows = "poetry2nix";
      };
      url = "github:farcaller/nixhelm";
    };

    nixidy = {
      inputs = {
        flake-utils.follows = "flake-utils";
        kubenix.follows = "kubenix";
        nix-kube-generators.follows = "nix-kube-generators";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:duck1123/nixidy?ref=feature/chmod";
    };

    nixos-anywhere = {
      inputs = {
        disko.follows = "disko";
        flake-parts.follows = "flake-parts";
        nixpkgs.follows = "nixpkgs";
        treefmt-nix.follows = "treefmt-nix";
      };
      url = "github:nix-community/nixos-anywhere";
    };

    nixos-facter-modules.url = "github:numtide/nixos-facter-modules";

    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    nixpkgs-lib.url = "github:nix-community/nixpkgs.lib";

    nur = {
      inputs = {
        flake-parts.follows = "flake-parts";
        nixpkgs.follows = "nixpkgs";
        treefmt-nix.follows = "treefmt-nix";
      };
      url = "github:nix-community/NUR";
    };

    poetry2nix = {
      inputs = {
        flake-utils.follows = "flake-utils";
        nix-github-actions.follows = "nix-github-actions";
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
        treefmt-nix.follows = "treefmt-nix";
      };
      url = "github:nix-community/poetry2nix";
    };

    pre-commit-hooks = {
      inputs = {
        flake-compat.follows = "flake-compat";
        gitignore.follows = "gitignore";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:cachix/git-hooks.nix";
    };

    sops-nix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:Mic92/sops-nix";
    };

    stylix = {
      inputs = {
        flake-compat.follows = "flake-compat";
        flake-parts.follows = "flake-parts";
        git-hooks.follows = "pre-commit-hooks";
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixpkgs";
        nur.follows = "nur";
        systems.follows = "systems";
      };
      url = "github:danth/stylix";
    };

    systems.url = "github:nix-systems/default";

    treefmt-nix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:numtide/treefmt-nix";
    };
  };

  outputs = { colmena, disko, flake-utils, home-manager, hyprpanel, nixpkgs
    , nixos-facter-modules, sops-nix, stylix, ... }@inputs:
    let
      inherit (flake-utils.lib) eachSystemMap defaultSystems;
      inherit (nixpkgs.lib) nixosSystem;
      inherit (home-manager.lib) homeManagerConfiguration;
      eachDefaultSystemMap = eachSystemMap defaultSystems;
      identities = rec {
        deck = {
          inherit (duck) name email gpgKey;
          username = "deck";
        };
        drenfer = {
          inherit (duck) gpgKey;
          name = "Daniel E. Renfer";
          username = "drenfer";
          email = "drenfer@vallen.com";
        };
        duck = {
          name = "Duck Nebuchadnezzar";
          username = "duck";
          email = "duck@kronkltd.net";
          gpgKey = "9564904D297DBF3C";
        };
      };
      config = {
        deck = {
          inherit (identities.deck) email gpgKey name username;
          hostname = "steamdeck";
        };
        drenfer = {
          inherit (identities.drenfer) email gpgKey name username;
          hostname = "vavirl-pw0bwnq8";
        };
        duck = {
          inherit (identities.duck) email gpgKey name username;
          hostname = "powerspecnix";
        };
        inspernix = {
          inherit (identities.duck) email gpgKey name username;
          hostname = "inspernix";
        };
      };
      defaultTZ = "America/Detroit";
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        # May the FOSS gods take mercy upon me
        config.allowUnfree = true;
        overlays = [ inputs.hyprpanel.overlay ];
      };
    in {
      colmenaHive = colmena.lib.makeHive {
        meta.nixpkgs = import nixpkgs { inherit system; };

        piNodeA = {
          boot.isContainer = true;
          deployment.targetHost = "pinodea";
          time.timeZone = defaultTZ;
        };
      };

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
            stylix.homeModules.stylix
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
            stylix.homeModules.stylix
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
            stylix.homeModules.stylix
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
            stylix.homeModules.stylix
            ./hosts/inspernix/home-for-flake.nix
          ];
        };
      };

      nixosConfigurations = {
        inspernix = nixosSystem {
          modules = [
            ./hosts/inspernix/configuration.nix
            disko.nixosModules.disko
            sops-nix.nixosModules.sops
          ];
          specialArgs = {
            inherit inputs;
            identity = config.duck;
          };
          system = "x86_64-linux";
        };
        powerspecnix = nixosSystem {
          modules = [
            ./hosts/powerspecnix/configuration.nix
            disko.nixosModules.disko
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
