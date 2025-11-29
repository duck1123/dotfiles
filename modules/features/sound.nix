{ ... }: {
  flake.types.generic.feature-options.sound = { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in simpleFeature "sound feature";

  flake.modules.nixos.sound-feature = { config, lib, ... }: {
    config = lib.mkIf config.host.features.sound.enable {
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
  };
}

