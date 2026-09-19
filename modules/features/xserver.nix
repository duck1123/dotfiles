_: {
  features.xserver = {
    nixos =
      { config, ... }:
      {
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
