{ ... }: {
  flake.types.generic.feature-options.radio = { inputs, lib }:
    let inherit (inputs.self.types.generic) simpleFeature;
    in simpleFeature { inherit inputs lib; } "radio feature";

  flake.modules.homeManager.radio = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.radio.enable {
      home.packages = with pkgs; [
        cubicsdr
        gnuradio
        gqrx
        rtl-sdr
        sdr-j-fm
        sdrangel
        sdrpp
      ];
    };
  };
}
