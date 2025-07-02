{ host, lib, pkgs, ... }: {
  config = lib.mkIf host.features.docker.enable {
    home.packages = [ pkgs.docker ];
    virtualisation.docker.enable = true;
  };
}
