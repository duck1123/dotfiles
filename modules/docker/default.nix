{ host, lib, ... }: {
  options = {
    features.docker.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable docker";
    };
  };

  config = lib.mkIf host.features.docker.enable {
    virtualisation.docker.enable = true;
  };
}
