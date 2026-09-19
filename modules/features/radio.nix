_: {
  features.radio = {
    homeManager =
      { pkgs, ... }:
      {
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

    nixos = _: {
      hardware.rtl-sdr.enable = true;
    };
  };
}
