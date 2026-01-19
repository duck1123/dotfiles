{ ... }:
{
  flake.types.generic.feature-options.tailscale =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "tailscale feature";

  flake.modules.nixos.tailscale-feature =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf config.host.features.tailscale.enable {
        services.tailscale.enable = true;
        # Disable tests to avoid test failures in 1.86.4
        services.tailscale.package = pkgs.tailscale.overrideAttrs (oldAttrs: {
          doCheck = false;
        });
      };
    };
}
