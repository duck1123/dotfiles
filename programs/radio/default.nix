{ host, lib, pkgs, ... }: {
  options = {
    features.radio.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable radio";
    };
  };

  config = lib.mkIf host.features.radio.enable {
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
