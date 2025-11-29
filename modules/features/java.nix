{ ... }: {
  flake.types.generic.feature-options.java = { inputs, lib }:
    let inherit (inputs.self.types.generic) simpleFeature;
    in simpleFeature { inherit inputs lib; } "java feature";

  flake.modules.homeManager.java = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.java.enable {
      home.packages = with pkgs; [ jdk ];
    };
  };
}

