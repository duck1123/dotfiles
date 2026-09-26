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
        };

        pulseaudio.enable = false;
      };

      users.users.${config.host.identity.username}.extraGroups = [
        "jackaudio"
        "realtime"
      ];
    };
}
