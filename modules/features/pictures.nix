{ ... }: {
  flake.modules.homeManager.pictures = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.pictures.enable {
      home.packages = with pkgs; [ digikam gimp ];
    };
  };
}

