{ inputs, ... }:
let
  pkgs = inputs.pkgs;
  wildvine-unzip = inputs.wildvine-unzip;
in {
  environment.systemPackages = with pkgs;
    [
      chromium
      # (runCommand "chromium-with-widevine" {} ''
      #   mkdir -p $out/lib/chromium
      #   cp ${widevine-unzip}/libwidevinecdm.so $out/lib/chromium/
      # '')
    ];

  services.xserver.windowManager.chromium = {
    enable = true;
    extraFlags = [
      "--enable-widevine"
      "--widevine-path=${widevine-unzip}/libwidevinecdm.so"
    ];
  };
}
