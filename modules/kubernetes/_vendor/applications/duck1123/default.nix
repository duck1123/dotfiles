{ ... }:
{
  flake.nixidyApps.duck1123 =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      name = "duck1123";
      labels = {
        "app.kubernetes.io/name" = name;
      };

      # ── Runtime ──────────────────────────────────────────────────────────────
      # The site itself (pubkey, relays, NIP-05 well-known file) lives as a real
      # npm/Vite project at applications/duck1123-site/, built via the
      # `duck1123-runtime` flake package (modules/pkgs/duck1123-site.nix,
      # symlinkJoin of the site + pkgs.python3 so /nix/var/result has both the
      # built static files and a `bin/python3` for server.py to run). Its
      # output store path is resolved right here at `nur switch` time (forcing
      # a local build as part of the activation package) and passed to nix-csi
      # via the CSI driver's per-system storePath convention (volumeAttributes
      # keyed by Nix system string, e.g. "x86_64-linux" -- same mechanism
      # applications/nix-csi.nix's builder pod uses for its init-store volume)
      # rather than a nixExpr string. This isn't just style: nix-csi evaluates
      # nixExpr without --impure, and builtins.storePath is rejected in pure
      # eval, so embedding the literal path inside nixExpr's source text
      # hard-fails NodePublishVolume ("Failed to build Nix expression") --
      # the storePath attribute instead gets handed to `nix build` as a plain
      # CLI installable, which substitutes fine. There's still no fallback:
      # this exact path must be pushed to Attic (`attic push nixos
      # ${self.packages.x86_64-linux.duck1123-runtime}`) as part of every
      # switch that changes it, or nix-csi has nothing to substitute it from.
      duck1123Runtime = self.packages.x86_64-linux.duck1123-runtime;

      serverScript = builtins.readFile ./server.py;
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

        extraResources =
          cfg:
          let
            port = 8080;
          in
          {
            configMaps.duck1123-server.data."server.py" = serverScript;

            deployments.${name}.spec = {
              selector.matchLabels = labels;
              template = {
                metadata.labels = labels;
                spec = {
                  containers = [
                    {
                      inherit name;
                      image = "ghcr.io/lillecarl/nix-csi/scratch:1.0.1";
                      command = [
                        "python3"
                        "/scripts/server.py"
                      ];
                      env = [
                        {
                          name = "PORT";
                          value = toString port;
                        }
                      ];
                      ports = [
                        {
                          containerPort = port;
                          name = "http";
                          protocol = "TCP";
                        }
                      ];
                      volumeMounts = [
                        {
                          name = "nix";
                          mountPath = "/nix";
                          subPath = "nix";
                        }
                        {
                          name = "scripts";
                          mountPath = "/scripts";
                        }
                      ];
                    }
                  ];
                  volumes = [
                    {
                      name = "nix";
                      csi = {
                        driver = "nix.csi.store";
                        volumeAttributes."x86_64-linux" = "${duck1123Runtime}";
                      };
                    }
                    {
                      name = "scripts";
                      configMap.name = "duck1123-server";
                    }
                  ];
                };
              };
            };

            # No Ingress: the public route (duck1123.com) is a Cloudflare Tunnel
            # Public Hostname pointed at this Service's cluster-internal DNS name
            # (duck1123.duck1123.svc.cluster.local:8080), configured in the
            # Cloudflare dashboard against the `cloudflared` app's tunnel.
            services.${name}.spec = {
              selector = labels;
              ports = [
                {
                  name = "http";
                  port = port;
                  targetPort = port;
                  protocol = "TCP";
                }
              ];
            };
          };
      };
}
