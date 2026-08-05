{ lib, ... }:
{
  options.devDefaults = {
    baseDomain = lib.mkOption {
      type = lib.types.str;
      default = "dev.kronkltd.net";
      description = "Base domain used by legacy/dev-only ingress hosts";
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
  };
}
