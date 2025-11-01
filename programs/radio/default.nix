{ config, lib, pkgs, ... }: {
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
}
