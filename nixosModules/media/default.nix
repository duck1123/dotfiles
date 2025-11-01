{ config, lib, ... }: {
  config = lib.mkIf config.host.features.media.server.enable {
    services.plex.enable = true;
  };
}
