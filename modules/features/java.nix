{ ... }: {
  flake.types.generic.feature-options.java = { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in simpleFeature "java feature";

  flake.modules.homeManager.java = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.java.enable {
      home.packages = with pkgs; [ jdk ];
    };
  };
}

