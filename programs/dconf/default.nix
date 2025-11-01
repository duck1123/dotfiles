{ config, lib, ... }: {
  config = lib.mkIf config.host.features.dconf.enable {
    dconf.settings = {
      "org/gnome/desktop/interface".color-scheme = "prefer-dark";

      "org/gnome/desktop/wm/preferences".button-layout =
        ":minimize,maximize,close";

      "apps/guake/general".default-shell = "/run/current-system/sw/bin/zsh";
    };
  };
}
