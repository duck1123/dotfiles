{ host, lib, ... }: {
  config =
    lib.mkIf host.features.touch.enable { services.libinput.enable = true; };
}
