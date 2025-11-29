{ ... }: {
  flake.types.generic.feature-options.email = { inputs, lib }:
    let inherit (inputs.self.types.generic) simpleFeature;
    in simpleFeature { inherit inputs lib; } "email feature";

  flake.modules.homeManager.email = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.email.enable {
      home.packages = with pkgs; [ thunderbird ];
    };
  };
}

