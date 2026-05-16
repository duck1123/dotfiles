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
        services.tailscale = {
          enable = true;
          # Prevent tailscaled from pushing its split-DNS nameserver (a Tailscale
          # infrastructure IP only reachable from Tailscale IPs) directly into
          # systemd-resolved, which would query it from the LAN IP and get REFUSED.
          # Instead we point resolved at 100.100.100.100 (tailscaled's local proxy)
          # which routes queries through the tunnel from the correct Tailscale IP.
          extraUpFlags = [ "--accept-dns=false" ];
        };

        services.resolved.settings.Resolve = {
          DNS = "100.100.100.100";
          Domains = "~ts.net";
        };
      };
    };
}
