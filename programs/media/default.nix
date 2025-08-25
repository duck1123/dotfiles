{ host, lib, pkgs, ... }: {
  config = lib.mkIf host.features.media.enable {
    home.packages = with pkgs; [
      kodi
      plex
      # plex-media-player
      vlc
      # yt-dlp
    ];
  };
}
