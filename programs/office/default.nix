{ host, lib, pkgs, ... }: {
  config = lib.mkIf host.features.office.enable {
    home.packages = with pkgs;
      [
        # gnumeric
        teams-for-linux
        # zoom-us
      ];
  };
}
