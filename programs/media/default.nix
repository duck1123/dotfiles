{ config, lib, pkgs, ... }: {
  config = lib.mkIf config.host.features.media.enable {
    home.packages = with pkgs; [ plex vlc ];
    programs.kodi.enable = false;
  };
}
