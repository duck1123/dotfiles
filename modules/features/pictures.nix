{ ... }: {
  flake.types.generic.feature-options.pictures = { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in simpleFeature "pictures feature";

  flake.modules.homeManager.pictures = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.pictures.enable {
      home.packages = with pkgs; [ digikam gimp ];
    };
  };
}

