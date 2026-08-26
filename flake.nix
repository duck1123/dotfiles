{
  description = "Duck's system configuration";

  nixConfig = {
    allow-import-from-derivation = true;
    extra-experimental-features = "nix-command flakes";
    extra-substituters = [
      "https://duck1123.cachix.org"
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "duck1123.cachix.org-1:Cj3r3BH7Xuy0zFWy8V/VIB3F7+Gi1m9HB302E9UGV3E="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  inputs = {
    attic = {
      inputs.flake-compat.follows = "flake-compat";
      inputs.flake-parts.follows = "flake-parts";
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:zhaofengli/attic";
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
        attic.follows = "attic";
        flake-compat.follows = "flake-compat";
        flake-parts.follows = "flake-parts";
        flake-utils.follows = "flake-utils";
        import-tree.follows = "import-tree";
        nix-csi.follows = "nix-csi";
        nix-kube-generators.follows = "nix-kube-generators";
        nixidy.follows = "nixidy";
        nixhelm.follows = "nixhelm";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:duck1123/k3s-fleetops";
    };

    nix-csi = {
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.treefmt-nix.follows = "treefmt-nix";
      url = "github:Lillecarl/nix-csi";
    };

    nix-kube-generators.url = "github:farcaller/nix-kube-generators";

    nixhelm = {
      inputs = {
        flake-utils.follows = "flake-utils";
        nix-kube-generators.follows = "nix-kube-generators";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:farcaller/nixhelm";
    };

    nixidy = {
      inputs = {
        flake-utils.follows = "flake-utils";
        nix-kube-generators.follows = "nix-kube-generators";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:arnarg/nixidy";
    };

    nixos-wsl = {
      inputs = {
        flake-compat.follows = "flake-compat";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:nix-community/NixOS-WSL";
    };

    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    nixpkgs-lib.url = "github:nix-community/nixpkgs.lib";

    nur = {
      inputs = {
        flake-parts.follows = "flake-parts";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:nix-community/NUR";
    };

    nur-taskrunner = {
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:duck1123/nur";
    };

    pre-commit-hooks = {
      inputs = {
        flake-compat.follows = "flake-compat";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:cachix/git-hooks.nix";
    };

    sddm-sugar-candy-nix = {
      inputs = {
        flake-parts.follows = "flake-parts";
        nixpkgs.follows = "nixpkgs";
      };
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
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
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

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } (inputs.import-tree ./modules);
}
