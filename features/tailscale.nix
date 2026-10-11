{
  description = "Tailscale feature";

  extraOptions =
    { lib, ... }:
    {
      advertiseRoutes = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = ''
          CIDR subnets to advertise into the Tailnet as subnet routes.
          When non-empty, enables IP forwarding and passes --advertise-routes to tailscale up.
          Routes still require approval in the Tailscale admin console.
        '';
      };
    };

  nixos =
    { config, lib, pkgs, ... }:
    let
      cfg = config.host.features.tailscale;
      setFlags = [
        "--accept-dns=false"
        "--accept-routes=true"
      ] ++ lib.optional (cfg.advertiseRoutes != [ ]) (
        "--advertise-routes=" + lib.concatStringsSep "," cfg.advertiseRoutes
      );
    in
    {
      services.tailscale = {
        enable = true;
        useRoutingFeatures = lib.mkIf (cfg.advertiseRoutes != [ ]) "server";
      };

      # services.tailscale.extraUpFlags only ever gets passed to `tailscale up`
      # by the tailscaled-autoconnect service, which only exists when
      # authKeyFile is set. We log in interactively (no authkey), so that unit
      # never runs and the flags below would otherwise never take effect.
      # `tailscale set` applies preferences to an already-registered node
      # without triggering the interactive login flow, so run it ourselves on
      # every boot/switch to keep prefs in sync with this config.
      systemd.services.tailscale-set-flags = {
        description = "Apply Tailscale preferences";
        after = [ "tailscaled.service" ];
        wants = [ "tailscaled.service" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
        };
        # Retry for a bit: right after a fresh install tailscaled may not be
        # logged in yet, in which case `tailscale set` fails until the user
        # runs an interactive `tailscale up` themselves.
        script = ''
          for i in $(seq 1 10); do
            if ${pkgs.tailscale}/bin/tailscale set ${lib.escapeShellArgs setFlags}; then
              exit 0
            fi
            sleep 3
          done
          exit 1
        '';
      };

      services.resolved.settings.Resolve = {
        # Prevent tailscaled from pushing its split-DNS nameserver (a Tailscale
        # infrastructure IP only reachable from Tailscale IPs) directly into
        # systemd-resolved, which would query it from the LAN IP and get REFUSED.
        # Instead we point resolved at 100.100.100.100 (tailscaled's local proxy)
        # which routes queries through the tunnel from the correct Tailscale IP.
        DNS = "100.100.100.100";
        Domains = "~ts.net";
      };
    };
}
