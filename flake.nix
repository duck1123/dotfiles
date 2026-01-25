{
  description = "Duck's system configuration";

  nixConfig = {
    allow-import-from-derivation = true;
    extra-experimental-features = "nix-command flakes";
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

    haumea = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/haumea";
    };

    home-manager = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/home-manager";
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
