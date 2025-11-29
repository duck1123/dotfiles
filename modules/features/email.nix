{ ... }: {
  flake.types.generic.feature-options.email = { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in simpleFeature "email feature";

  flake.modules.homeManager.email = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.email.enable {
      home.packages = with pkgs; [ thunderbird ];
    };
  };
}

