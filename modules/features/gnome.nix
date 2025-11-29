{ ... }: {
  flake.types.generic.feature-options.gnome = { inputs, lib }:
    let inherit (inputs.self.types.generic) simpleFeature;
    in simpleFeature { inherit inputs lib; } "gnome feature";

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

