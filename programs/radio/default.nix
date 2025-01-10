{ pkgs, ... }: {
  home.packages = with pkgs; [
    cubicsdr
    gnuradio
    gqrx
    rtl-sdr
    sdr-j-fm
    sdrangel
    sdrpp
  ];
}
