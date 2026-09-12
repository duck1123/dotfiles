{ lib, ... }:
{
  options.devDefaults = {
    baseDomain = lib.mkOption {
      type = lib.types.str;
      default = "dev.kronkltd.net";
      description = "Base domain used by legacy/dev-only ingress hosts";
    };

    homeDomain = lib.mkOption {
      type = lib.types.str;
      default = "home.kronkltd.net";
      description = "LAN-facing domain fronted by the Traefik ingress";
    };

    tailDomain = lib.mkOption {
      type = lib.types.str;
      default = "bearded-snake.ts.net";
      description = "Tailscale MagicDNS domain for tailscale-routed ingresses";
    };

    clusterIssuer = lib.mkOption {
      type = lib.types.str;
      default = "letsencrypt-prod";
      description = "Default cert-manager ClusterIssuer for LAN ingresses";
    };

    nasHost = lib.mkOption {
      type = lib.types.str;
      default = "192.168.0.124";
      description = "NAS host/IP used for NFS mounts";
    };

    nasBase = lib.mkOption {
      type = lib.types.str;
      default = "/volume1";
      description = "Base NFS export path on the NAS";
    };

    enableLogging = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Toggle to enable/disable all logging components (loki/promtail/grafana Loki datasource)";
    };
  };
}
