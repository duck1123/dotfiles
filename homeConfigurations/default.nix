{ hosts, inputs, pkgs, system, ... }:
# Home configurations
# Accessible via 'home-manager'
let
  core =
    [ inputs.stylix.homeModules.stylix inputs.zen-browser.homeModules.beta ];
  inherit (inputs.home-manager.lib) homeManagerConfiguration;
in {
  drenfer = homeManagerConfiguration {
    inherit pkgs;
    extraSpecialArgs = {
      inherit hosts inputs system;
      host = hosts.vallenpc;
    };
    modules = core ++ [ ../hosts/vavirl-pw0bwnq8/home.nix ];
  };

  deck = homeManagerConfiguration {
    inherit pkgs;
    extraSpecialArgs = {
      inherit hosts inputs system;
      host = hosts.steamdeck;
    };
    modules = core ++ [ ../hosts/steamdeck/home.nix ];
  };

  "duck@inspernix" = homeManagerConfiguration {
    inherit pkgs;
    extraSpecialArgs = {
      inherit hosts inputs system;
      host = hosts.inspernix;
    };
    modules = core ++ [ ../hosts/inspernix/home.nix ];
  };

  "duck@nasnix" = homeManagerConfiguration {
    inherit pkgs;
    extraSpecialArgs = {
      inherit hosts inputs system;
      host = hosts.nasnix;
    };
    modules = core ++ [ ../hosts/nasnix/home.nix ];
  };

  "duck@powerspecnix" = homeManagerConfiguration {
    inherit pkgs;
    extraSpecialArgs = {
      inherit hosts inputs system;
      host = hosts.powerspecnix;
    };
    modules = core ++ [ ../hosts/powerspecnix/home.nix ];
  };
}
