{ host, lib, ... }: {
  config = lib.mkIf host.features.xserver.enable {
    services.xserver = {
      enable = true;

      xkb = {
        layout = "us";
        variant = "";
      };
    };

    xdg.portal = {
      enable = true;
      config.common.default = "*";
    };
  };
}
