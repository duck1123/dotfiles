{ inputs, ... }:
let
  inherit (inputs.home-manager.lib) homeManagerConfiguration;
  inherit (inputs.self.modules) home-manager;
  pkgs = import inputs.nixpkgs {
    config.allowUnfree = true;
    system = "x86_64-linux";
  };
in {
  flake.homeConfigurations = {
    "drenfer@VAVIRL-PW0BWNQ8" = homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = { inherit inputs pkgs; };
      modules = with home-manager; [ core vavirl-pw0bwnq8 ];
    };

    "deck@steamdeck" = homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = { inherit inputs pkgs; };
      modules = with home-manager; [ core steamdeck ];
    };

    "duck@inspernix" = homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = { inherit inputs pkgs; };
      modules = with home-manager; [ core inspernix ];
    };

    "duck@nasnix" = homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = { inherit inputs pkgs; };
      modules = with home-manager; [ core nasnix ];
    };

    "duck@powerspecnix" = homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = { inherit inputs pkgs; };
      modules = with home-manager; [ core powerspecnix ];
    };
  };
}
