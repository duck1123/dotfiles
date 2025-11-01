{ config, lib, ... }: {
  config = lib.mkIf config.host.features.docker.enable {
    virtualisation.docker.enable = true;
  };
}
