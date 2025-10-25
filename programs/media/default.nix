{ host, lib, pkgs, ... }: {
  options = {
    features.media.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable media";
    };
  };

  config = lib.mkIf host.features.media.enable {
    home.packages = with pkgs; [ plex vlc ];
    programs.kodi.enable = false;
  };
}
