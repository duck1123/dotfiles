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
      acceptRoutes = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          Use subnet routes advertised by other nodes (--accept-routes).
          Leave off on hosts on the home LAN: tailscale's routing table wins
          over the main one, so nasnix's 192.168.0.0/24 route would send LAN
          traffic (k3s's flannel VXLAN included) into the tunnel.
        '';
      };
    };

  nixos =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      cfg = config.host.features.tailscale;
      setFlags = [
        "--accept-dns=false"
        "--accept-routes=${lib.boolToString cfg.acceptRoutes}"
      ]
      ++ lib.optional (cfg.advertiseRoutes != [ ]) (
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

      # NetworkManager would otherwise treat tailscale0 as an external
      # connection and could reset the per-link DNS set below.
      networking.networkmanager.unmanaged = lib.mkIf config.networking.networkmanager.enable [
        config.services.tailscale.interfaceName
      ];

      # Send only *.ts.net lookups to tailscaled's local MagicDNS proxy
      # (100.100.100.100), and only via tailscale0. With --accept-dns=false
      # tailscaled has no upstream resolvers, so putting it in the global
      # resolved config (as this used to) made it SERVFAIL every ordinary
      # lookup, racing the router's DNS. Per-link settings vanish when
      # tailscaled recreates the interface, so re-apply on every restart.
      systemd.services.tailscale-dns = lib.mkIf config.services.resolved.enable {
        description = "Route *.ts.net lookups to Tailscale's resolver";
        after = [
          "tailscaled.service"
          "systemd-resolved.service"
        ];
        partOf = [ "tailscaled.service" ];
        wantedBy = [ "tailscaled.service" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
        };
        script =
          let
            iface = config.services.tailscale.interfaceName;
            resolvectl = "${config.systemd.package}/bin/resolvectl";
          in
          ''
            for i in $(seq 1 30); do
              [ -e /sys/class/net/${iface} ] && break
              sleep 1
            done
            ${resolvectl} dns ${iface} 100.100.100.100
            ${resolvectl} domain ${iface} '~ts.net'
            ${resolvectl} default-route ${iface} false
          '';
      };
    };
}
