{ config, lib, ... }: {
  config = lib.mkIf config.host.features.gaming.enable {
    programs.steam.enable = true;
  };
}
