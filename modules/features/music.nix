{ ... }: {
  flake.modules.homeManager.music = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.music.enable {
      home.packages = with pkgs; [
        # Multi-track hard disk recording software
        ardour

        # obs-studio

        # Audio plugin host
        carla

        guitarix

        # Advanced drum machine
        hydrogen

        # DAW similar to FL Studio (music production software)
        # lmms

        musescore

        qjackctl

        # Modern tracker-based DAW
        # renoise

        supercollider

        # Old-school 4-oscillator subtractive polyphonic synthesizer with stereo fx
        synthv1

        # vcv-rack

        # Virtual MIDI Piano Keyboard
        vmpk
      ];
    };
  };
}

