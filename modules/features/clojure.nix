{ ... }: {
  flake.types.generic.feature-options.clojure = { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in simpleFeature "clojure feature";

  flake.modules.homeManager.clojure = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.clojure.enable {
      home.packages = with pkgs; [ babashka bbin clojure clojure-lsp jet ];
    };
  };
}

