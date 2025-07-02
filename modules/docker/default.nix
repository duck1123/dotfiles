{ host, lib, pkgs, ... }: {
  config = lib.mkIf host.feature.docker.enable {
    home.packages = [ pkgs.docker ];
  };
}
