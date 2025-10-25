{ host, lib, ... }: {
  options = {
    features.media.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable media";
    };
  };

  config =
    lib.mkIf host.features.media.server.enable { services.plex.enable = true; };
}
