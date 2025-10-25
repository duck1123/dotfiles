{ host, lib, pkgs, ... }: {
  options = {
    features.office.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable office";
    };
  };

  config = lib.mkIf host.features.office.enable {
    home.packages = with pkgs;
      [
        # gnumeric
        teams-for-linux
        # zoom-us
      ];
  };
}
