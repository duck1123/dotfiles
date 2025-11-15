{ ... }: {
  flake.modules.homeManager.flipper = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.flipper.enable {
      home.packages = with pkgs; [ qFlipper ];
    };
  };
}

