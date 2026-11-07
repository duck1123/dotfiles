{
  homeManager =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        # amidi (sysex send/receive), aconnect, amixer/alsamixer
        alsa-utils

        # Multi-track hard disk recording software
        ardour

        # obs-studio

        # Audio plugin host
        carla

        # DX7 FM synth plugin and patch editor (FM-1 sysex banks)
        dexed

        guitarix

        # Advanced drum machine
        hydrogen

        # DAW similar to FL Studio (music production software)
        # lmms

        musescore

        # PipeWire patchbay for audio/MIDI routing
        qpwgraph

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
}
