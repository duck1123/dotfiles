{
  description = "Duck's system configuration";

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

    devshell = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:numtide/devshell";
    };

    flake-compat.url = "github:edolstra/flake-compat";

    flake-file.url = "github:vic/flake-file";

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

  outputs = inputs:
    let
      identities = import ./identities.nix { };
      system = "x86_64-linux";
      pkgs = import inputs.nixpkgs {
        inherit system;
        # May the FOSS gods take mercy upon me
        config.allowUnfree = true;
        overlays = [ ];
      };
      hosts = import ./hosts { inherit identities system; };
      homeConfigurations =
        import ./homeConfigurations { inherit hosts inputs pkgs system; };
      nixosConfigurations =
        import ./nixosConfigurations { inherit hosts inputs system; };
      devShells = import ./devShells { inherit inputs; };
    in devShells // {
      inherit homeConfigurations nixosConfigurations;

      # FIXME: Can't use `nix shell` without this.
      packages.${system}.default = pkgs.cowsay;
    };
}
