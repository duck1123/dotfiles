{ ... }: {
  flake.types.generic.syncthing-submodule = { inputs, lib, ... }:
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in with lib;
    types.submodule {
      options = {
        enable = mkOption {
          type = types.bool;
          default = false;
          description = "Enable syncthing";
        };

        shares = mkOption {
          type = types.submodule {
            options = {
              camera = simpleFeature "camera share";
              keepass = simpleFeature "keepass share";
              org-roam = simpleFeature "org-roam share";
              renpy = simpleFeature "renpy share";
            };
          };
          default = { };
          description = "Syncthing shares configuration";
        };
      };
    };
}
