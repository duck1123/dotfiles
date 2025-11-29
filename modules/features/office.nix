{ ... }: {
  flake.types.generic.feature-options.office = { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in simpleFeature "office feature";

  flake.modules.homeManager.office = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.office.enable {
      home.packages = with pkgs;
        [
          # gnumeric
          teams-for-linux
          # zoom-us
        ];
    };
  };
}

