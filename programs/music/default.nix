{ pkgs, ... }: {
  home.packages = with pkgs; [
    # Multi-track hard disk recording software
    ardour

    # Audio plugin host
    carla

    guitarix

    # Advanced drum machine
    hydrogen

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
}
