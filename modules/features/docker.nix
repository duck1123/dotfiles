{ ... }: {
  flake.types.generic.feature-options.docker = { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in simpleFeature "docker feature";

  flake.modules.nixos.docker-feature = { config, lib, ... }: {
    config = lib.mkIf config.host.features.docker.enable {
      virtualisation.docker.enable = true;
    };
  };
}

