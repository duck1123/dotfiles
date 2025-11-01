{ config, lib, ... }: {
  config = lib.mkIf config.host.features.touch.enable {
    services.libinput.enable = true;
  };
}
