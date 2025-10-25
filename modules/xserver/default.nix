{ host, lib, ... }: {
  options = {
    features.xserver.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable xserver";
    };
  };

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
