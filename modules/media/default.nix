{ host, lib, ... }: {
  config = lib.mkIf host.features.media.server.enable {
    services.plex.enable = true;
  };
}
