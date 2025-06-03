{ pkgs, ... }: {
  options.modules.radio.enable = mkEnableOption "radio tools";

  config = mkIf config.modules.radio.enable {
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
