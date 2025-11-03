{ inputs, ... }:
let
  inherit (inputs.home-manager.lib) homeManagerConfiguration;
  system = "x86_64-linux";
  hosts = import ../../hosts/default.nix { };
  pkgs = import inputs.nixpkgs {
    inherit system;
    config = { allowUnfree = true; };
  };
  core = [
    inputs.stylix.homeModules.stylix
    inputs.zen-browser.homeModules.beta
    { inherit hosts; }
    ../../nixosModules/flakeModules
  ];
in {
  flake.homeConfigurations = {
    "drenfer@VAVIRL-PW0BWNQ8" = homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = { inherit inputs; };
      modules = core
        ++ [ { host = hosts.vallenpc; } ../../hosts/vavirl-pw0bwnq8/home.nix ];
    };

    "deck@steamdeck" = homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = { inherit inputs; };
      modules = core
        ++ [ { host = hosts.steamdeck; } ../../hosts/steamdeck/home.nix ];
    };

    "duck@inspernix" = homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = { inherit inputs; };
      modules = core
        ++ [ { host = hosts.inspernix; } ../../hosts/inspernix/home.nix ];
    };

    "duck@nasnix" = homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = { inherit inputs; };
      modules = core
        ++ [ { host = hosts.nasnix; } ../../hosts/nasnix/home.nix ];
    };

    "duck@powerspecnix" = homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = { inherit inputs; };
      modules = core
        ++ [ { host = hosts.powerspecnix; } ../../hosts/powerspecnix/home.nix ];
    };
  };
}
