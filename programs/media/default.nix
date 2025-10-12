{ host, lib, pkgs, ... }: {
  config = lib.mkIf host.features.media.enable {
    home.packages = with pkgs; [ plex vlc ];
    programs.kodi.enable = false;
  };
}
