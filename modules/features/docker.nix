{ ... }: {
  flake.modules.nixos.docker-feature = { config, lib, ... }: {
    config = lib.mkIf config.host.features.docker.enable {
      virtualisation.docker.enable = true;
    };
  };
}

