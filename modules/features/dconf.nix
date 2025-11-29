{ ... }: {
  flake.types.generic.feature-options.dconf = { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in simpleFeature "dconf feature";

  flake.modules.homeManager.dconf = { config, lib, ... }: {
    config = lib.mkIf config.host.features.dconf.enable {
      dconf.settings = {
        "org/gnome/desktop/interface".color-scheme = "prefer-dark";

        "org/gnome/desktop/wm/preferences".button-layout =
          ":minimize,maximize,close";

        "apps/guake/general".default-shell = "/run/current-system/sw/bin/zsh";
      };
    };
  };
}

