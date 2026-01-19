{ ... }:
{
  flake.types.generic.feature-options.docker =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "docker feature";

  flake.modules.nixos.docker-feature =
    { config, lib, ... }:
    {
      config = lib.mkIf config.host.features.docker.enable {
        virtualisation.docker.enable = true;
      };
    };
}
