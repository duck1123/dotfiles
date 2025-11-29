{ ... }: {
  flake.types.generic.feature-options.chm = { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in simpleFeature "chm feature";

  flake.modules.homeManager.chm = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.chm.enable {
      home.packages = with pkgs; [ kchmviewer ];
    };
  };
}

