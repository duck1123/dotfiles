{ host, lib, ... }: {
  config = lib.mkIf host.features.sound.enable {
    security.rtkit.enable = true;

    services = {
      pipewire = {
        enable = true;

        alsa = {
          enable = true;
          support32Bit = true;
        };

        jack.enable = true;
        pulse.enable = true;
      };

      pulseaudio.enable = false;
    };
  };
}
