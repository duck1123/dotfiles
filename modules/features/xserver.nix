{ ... }: {
  flake.modules.nixos.xserver-feature = { config, lib, ... }: {
    config = lib.mkIf config.host.features.xserver.enable {
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
  };
}

