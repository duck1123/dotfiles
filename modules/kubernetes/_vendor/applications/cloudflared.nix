{ ... }:
{
  flake.nixidyApps.cloudflared =
    {
      config,
      lib,
      self,
      pkgs,
      ...
    }:
    with lib;
    let
      name = "cloudflared";
      labels = {
        "app.kubernetes.io/name" = name;
      };

      secretName = "cloudflared";

      # ── Runtime ──────────────────────────────────────────────────────────────
      # nix-csi evaluates this expression on the node and mounts the result at
      # /nix so its bin directory is on PATH inside the scratch container.
      # cloudflared dials out over TLS to Cloudflare's edge, so bundle cacert
      # alongside it (symlinkJoin) — plain pkgs.cloudflared has no CA store of
      # its own, unlike a flake package that already vendors one.
      nixExpr = ''
        let
          pkgs = import (builtins.fetchTree {
            type = "github";
            owner = "nixos";
            repo = "nixpkgs";
            ref = "nixos-unstable";
          }) {};
        in
        pkgs.symlinkJoin {
          name = "cloudflared-bundle";
          paths = [
            pkgs.cloudflared
            pkgs.cacert
          ];
        }
      '';
    in
    self.lib.mkArgoApp
      {
        inherit
          config
          lib
          self
          pkgs
          ;
      }
      {
        inherit name;

        extraOptions = {
          # Token from a Cloudflare Zero Trust tunnel (Networks → Tunnels →
          # Create a tunnel → Cloudflared connector). Hostname routing for the
          # tunnel is configured entirely in the Cloudflare dashboard's "Public
          # Hostname" tab, not here — this deployment only needs to authenticate
          # and stay connected.
          tunnelToken = mkOption {
            description = mdDoc "Cloudflare Tunnel token";
            type = types.str;
            default = "";
          };

          replicas = mkOption {
            description = mdDoc "Number of cloudflared replicas (Cloudflare load-balances across connected replicas of the same tunnel)";
            type = types.int;
            default = 2;
          };
        };

        sopsSecrets =
          cfg:
          optionalAttrs (cfg.tunnelToken != "") {
            ${secretName}.tunnelToken = cfg.tunnelToken;
          };

        extraResources =
          cfg:
          optionalAttrs (cfg.tunnelToken != "") {
            deployments.${name}.spec = {
              replicas = cfg.replicas;
              selector.matchLabels = labels;
              template = {
                metadata.labels = labels;
                spec = {
                  containers = [
                    {
                      inherit name;
                      image = "ghcr.io/lillecarl/nix-csi/scratch:1.0.1";
                      command = [ "cloudflared" ];
                      args = [
                        "tunnel"
                        "--no-autoupdate"
                        "run"
                        "--token"
                        "$(TUNNEL_TOKEN)"
                      ];
                      env = [
                        {
                          name = "TUNNEL_TOKEN";
                          valueFrom.secretKeyRef = {
                            name = secretName;
                            key = "tunnelToken";
                          };
                        }
                        {
                          name = "SSL_CERT_FILE";
                          value = "/nix/var/result/etc/ssl/certs/ca-bundle.crt";
                        }
                      ];
                      volumeMounts = [
                        {
                          name = "nix";
                          mountPath = "/nix";
                          subPath = "nix";
                        }
                      ];
                    }
                  ];
                  volumes = [
                    {
                      name = "nix";
                      csi = {
                        driver = "nix.csi.store";
                        volumeAttributes.nixExpr = nixExpr;
                      };
                    }
                  ];
                };
              };
            };
          };
      };
}
