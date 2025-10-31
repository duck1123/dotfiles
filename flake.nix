{
  description = "home-manager config";

  nixConfig = {
    extra-experimental-features = "nix-command flakes";
    # extra-substituters =
    #   [ "https://duck1123.cachix.org" "https://nix-community.cachix.org" ];
    # extra-trusted-public-keys = [
    #   "duck1123.cachix.org-1:Cj3r3BH7Xuy0zFWy8V/VIB3F7+Gi1m9HB302E9UGV3E="
    #   "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    # ];
  };

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

    import-tree.url = "github:vic/import-tree";

    k3s-fleetops = {
      inputs = {
        clj-nix.follows = "clj-nix";
        flake-parts.follows = "flake-parts";
        flake-utils.follows = "flake-utils";
        make-shell.follows = "make-shell";
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

    sddm-sugar-candy-nix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "gitlab:Zhaith-Izaliel/sddm-sugar-candy-nix";
    };

    sops-nix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:Mic92/sops-nix";
    };

    stylix = {
      inputs = {
        flake-parts.follows = "flake-parts";
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

    zen-browser = {
      inputs = {
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:0xc000022070/zen-browser-flake";
    };
  };

  outputs = { colmena, flake-utils, home-manager, nixpkgs, stylix, zen-browser
    , ... }@inputs:
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
          email = "daniel.renfer@vallen.com";
        };
        duck = {
          name = "Duck Nebuchadnezzar";
          username = "duck";
          email = "duck@kronkltd.net";
          gpgKey = "9564904D297DBF3C";
        };
      };
      defaultTZ = "America/Detroit";
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        # May the FOSS gods take mercy upon me
        config.allowUnfree = true;
        overlays = [ ];
      };
      hosts = {
        inspernix =
          import ./hosts/inspernix/default.nix { inherit system identities; };
        nasnix =
          import ./hosts/nasnix/default.nix { inherit system identities; };
        pixel8 =
          import ./hosts/pixel8/default.nix { inherit system identities; };
        powerspecnix = import ./hosts/powerspecnix/default.nix {
          inherit system identities;
        };
        steamdeck =
          import ./hosts/steamdeck/default.nix { inherit system identities; };
        vallenpc = import ./hosts/vavirl-pw0bwnq8/default.nix {
          inherit system identities;
        };
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
      homeConfigurations =
        let core = [ stylix.homeModules.stylix zen-browser.homeModules.beta ];
        in {
          drenfer = homeManagerConfiguration {
            inherit pkgs;
            extraSpecialArgs = {
              inherit hosts inputs system;
              host = hosts.vallenpc;
            };
            modules = core ++ [ ./hosts/vavirl-pw0bwnq8/home.nix ];
          };

          deck = homeManagerConfiguration {
            inherit pkgs;
            extraSpecialArgs = {
              inherit hosts inputs system;
              host = hosts.steamdeck;
            };
            modules = core ++ [ ./hosts/steamdeck/home.nix ];
          };

          "duck@inspernix" = homeManagerConfiguration {
            inherit pkgs;
            extraSpecialArgs = {
              inherit hosts inputs system;
              host = hosts.inspernix;
            };
            modules = core ++ [ ./hosts/inspernix/home.nix ];
          };

          "duck@nasnix" = homeManagerConfiguration {
            inherit pkgs;
            extraSpecialArgs = {
              inherit hosts inputs system;
              host = hosts.nasnix;
            };
            modules = core ++ [ ./hosts/nasnix/home.nix ];
          };

          "duck@powerspecnix" = homeManagerConfiguration {
            inherit pkgs;
            extraSpecialArgs = {
              inherit hosts inputs system;
              host = hosts.powerspecnix;
            };
            modules = core ++ [ ./hosts/powerspecnix/home.nix ];
          };
        };

      nixosConfigurations = {
        inspernix = nixosSystem {
          inherit (hosts.inspernix) system;
          modules = [ ./hosts/inspernix/configuration.nix ];
          specialArgs = {
            inherit hosts inputs system;
            host = hosts.inspernix;
          };
        };

        nasnix = nixosSystem {
          inherit (hosts.inspernix) system;
          modules = [ ./hosts/nasnix/configuration.nix ];
          specialArgs = {
            inherit hosts inputs system;
            host = hosts.nasnix;
          };
        };

        powerspecnix = nixosSystem {
          inherit (hosts.powerspecnix) system;
          modules = [ ./hosts/powerspecnix/configuration.nix ];
          specialArgs = {
            inherit hosts inputs system;
            host = hosts.powerspecnix;
          };
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
              cachix
              clojure
              pkgs.colmena
              emacs
              git
              pkgs.home-manager
              keepassxc
              kubectl
              nh
              nix
              nixpkgs-fmt
              nmap
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
