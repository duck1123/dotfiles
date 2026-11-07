{
  nixos =
    { config, ... }:
    {
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

          # Let the graph switch to 44.1 kHz for devices that only run at that
          # rate (e.g. the M-Vave FM-1) instead of resampling to 48 kHz.
          extraConfig.pipewire."10-rates"."context.properties"."default.clock.allowed-rates" = [
            44100
            48000
          ];
        };

        pulseaudio.enable = false;
      };

      users.users.${config.host.identity.username}.extraGroups = [
        "jackaudio"
        "realtime"
      ];
    };
}
