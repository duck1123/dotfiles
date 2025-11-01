{ config, lib, pkgs, ... }: {
  config = lib.mkIf config.host.features.office.enable {
    home.packages = with pkgs;
      [
        # gnumeric
        teams-for-linux
        # zoom-us
      ];
  };
}
