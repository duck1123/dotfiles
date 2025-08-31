{ host, lib, ... }: {
  config = lib.mkIf host.features.media.enable {

    services = { plex.enable = true; };
  };
}
