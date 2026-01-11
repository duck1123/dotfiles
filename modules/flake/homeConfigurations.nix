{ inputs, ... }:
let
  inherit (inputs.home-manager.lib) homeManagerConfiguration;
  inherit (inputs.self.modules) homeManager;
  pkgs = import inputs.nixpkgs {
    config.allowUnfree = true;
    system = "x86_64-linux";
  };
in {
  flake.homeConfigurations = {
    "drenfer@VAVIRL-PW0BWNQ8" = homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = { inherit inputs pkgs; };
      modules = with homeManager; [ base vavirl-pw0bwnq8 ];
    };

    "deck@steamdeck" = homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = { inherit inputs pkgs; };
      modules = with homeManager; [ base steamdeck ];
    };

    "duck@edgenix" = homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = { inherit inputs pkgs; };
      modules = with homeManager; [ base edgenix ];
    };

    "duck@inspernix" = homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = { inherit inputs pkgs; };
      modules = with homeManager; [ base inspernix ];
    };

    "duck@nasnix" = homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = { inherit inputs pkgs; };
      modules = with homeManager; [ base nasnix ];
    };

    "duck@powerspecnix" = homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = { inherit inputs pkgs; };
      modules = with homeManager; [ base powerspecnix ];
    };

    "duck@vidcentre" = homeManagerConfiguration {
      inherit pkgs;
      extraSpecialArgs = { inherit inputs pkgs; };
      modules = with homeManager; [ base vidcentre ];
    };
  };
}
