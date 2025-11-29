{ ... }: {
  flake.types.generic.feature-options.gnome = { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in simpleFeature "gnome feature";

  flake.modules.homeManager.gnome = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.gnome.enable {
      home.packages = with pkgs; [
        gnomeExtensions.appindicator
        gnomeExtensions.gsconnect
        # gnomeExtensions.topicons-plus
        guake
      ];
    };
  };
}

