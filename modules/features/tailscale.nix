{ ... }:
{
  flake.types.generic.feature-options.tailscale =
    { inputs, lib }:
    with lib;
    mkOption {
      type = types.submodule {
        options = {
          enable = mkEnableOption "Tailscale feature";

          advertiseRoutes = mkOption {
            type = types.listOf types.str;
            default = [ ];
            description = ''
              CIDR subnets to advertise into the Tailnet as subnet routes.
              When non-empty, enables IP forwarding and passes --advertise-routes to tailscale up.
              Routes still require approval in the Tailscale admin console.
            '';
          };
        };
      };
      default = { };
    };

  flake.modules.nixos.tailscale-feature =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      cfg = config.host.features.tailscale;
      routesFlag = lib.optionalString (cfg.advertiseRoutes != [ ]) (
        "--advertise-routes=" + lib.concatStringsSep "," cfg.advertiseRoutes
      );
    in
    {
      config = lib.mkIf cfg.enable {
        services.tailscale = {
          enable = true;
          # Prevent tailscaled from pushing its split-DNS nameserver (a Tailscale
          # infrastructure IP only reachable from Tailscale IPs) directly into
          # systemd-resolved, which would query it from the LAN IP and get REFUSED.
          # Instead we point resolved at 100.100.100.100 (tailscaled's local proxy)
          # which routes queries through the tunnel from the correct Tailscale IP.
          extraUpFlags = [ "--accept-dns=false" ] ++ lib.optional (routesFlag != "") routesFlag;
          useRoutingFeatures = lib.mkIf (cfg.advertiseRoutes != [ ]) "server";
        };

        services.resolved.settings.Resolve = {
          DNS = "100.100.100.100";
          Domains = "~ts.net";
        };
      };
    };
}
