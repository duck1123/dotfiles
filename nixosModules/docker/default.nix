{ host, lib, ... }: {
  config = lib.mkIf host.features.docker.enable {
    virtualisation.docker.enable = true;
  };
}
