{ host, lib, ... }: {
  options = {
    features.dconf.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable dconf";
    };
  };

  config = lib.mkIf host.features.dconf.enable {
    dconf.settings = {
      "org/gnome/desktop/interface".color-scheme = "prefer-dark";

      "org/gnome/desktop/wm/preferences".button-layout =
        ":minimize,maximize,close";

      "apps/guake/general".default-shell = "/run/current-system/sw/bin/zsh";
    };
  };
}
