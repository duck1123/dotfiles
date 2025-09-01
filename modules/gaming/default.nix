{ host, lib, ...}: {
  config = lib.mkIf host.features.gaming.enable {
    programs.steam.enable = true;
  };
}
