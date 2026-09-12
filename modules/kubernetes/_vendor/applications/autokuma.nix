{ ... }:
{
  flake.nixidyApps.autokuma =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      name = "autokuma";
      labels = {
        "app.kubernetes.io/name" = name;
        "app.kubernetes.io/instance" = name;
      };

      creds-secret = "autokuma-kuma-credentials";
      monitors-configmap = "autokuma-monitors";

      # autokuma is packaged upstream in nixpkgs (pkgs.autokuma) and its build
      # bundles the kuma-cli binary (`kuma`) alongside it — no packaging needed
      # here, just fetch a pinned nixpkgs at runtime like the other nix-csi apps.
      # plain pkgs.autokuma has no CA store of its own, but SSL_CERT_FILE below
      # points into this package's output regardless — bundle cacert (symlinkJoin)
      # so that path actually exists, same as cloudflared.nix. Without it, the
      # HTTP client hangs on connect() forever trying to load a missing cert
      # bundle, even against a plain http:// URL — silent, no log output at all.
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
          name = "autokuma-bundle";
          paths = [
            pkgs.autokuma
            pkgs.cacert
          ];
        }
      '';

      # Every mkArgoApp-based service exposes `monitoring.autokuma.*` (see
      # modules/lib/mkArgoApp.nix). Collect one static-monitor file per service
      # that both `enable`s itself and opts into monitoring, so this ConfigMap
      # (mounted as AutoKuma's `files` source) stays in sync with whatever this
      # repo currently has switched on.
      enabledMonitors = lib.filterAttrs (
        _: svc: (svc.enable or false) && (svc.monitoring.autokuma.enable or false)
      ) config.services;

      monitorData = lib.mapAttrs' (
        svcName: svc:
        let
          m = svc.monitoring.autokuma;
        in
        lib.nameValuePair "${svcName}.json" (
          builtins.toJSON (
            {
              inherit (m) type;
              name = m.displayName;
            }
            // lib.optionalAttrs (m.url != null) { inherit (m) url; }
            // m.extraSettings
          )
        )
      ) enabledMonitors;
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
          kuma.url = mkOption {
            description = mdDoc "Uptime Kuma URL (Socket.IO endpoint) AutoKuma connects to.";
            type = types.str;
            default = "http://uptime-kuma.uptime-kuma:3001";
          };

          kuma.username = mkOption {
            description = mdDoc "Uptime Kuma login username. Stored in a SOPS secret.";
            type = types.str;
            default = "";
          };

          kuma.password = mkOption {
            description = mdDoc "Uptime Kuma login password. Stored in a SOPS secret.";
            type = types.str;
            default = "";
          };

        };

        sopsSecrets =
          cfg:
          optionalAttrs (cfg.kuma.username != "" && cfg.kuma.password != "") {
            ${creds-secret} = {
              USERNAME = cfg.kuma.username;
              PASSWORD = cfg.kuma.password;
            };
          };

        extraResources = cfg: {
          configMaps.${monitors-configmap}.data = monitorData;

          deployments.${name}.spec = {
            replicas = 1;
            selector.matchLabels = labels;
            template = {
              metadata.labels = labels;
              spec = {
                containers = [
                  {
                    inherit name;
                    image = "ghcr.io/lillecarl/nix-csi/scratch:1.0.1";
                    command = [ "autokuma" ];
                    env = [
                      {
                        # Default log level is silent past startup migrations — no
                        # connect/sync/error output at all, which made a genuine
                        # hang indistinguishable from a quiet success. Scoped to
                        # skip sled's very noisy pagecache/iobuf DEBUG spam.
                        name = "RUST_LOG";
                        value = "info,autokuma=debug,kuma_client=debug";
                      }
                      {
                        name = "AUTOKUMA__KUMA__URL";
                        value = cfg.kuma.url;
                      }
                      {
                        name = "AUTOKUMA__DOCKER__ENABLED";
                        value = "false";
                      }
                      {
                        name = "AUTOKUMA__KUBERNETES__ENABLED";
                        value = "false";
                      }
                      {
                        name = "AUTOKUMA__STATIC_MONITORS";
                        value = "/data/monitors";
                      }
                      {
                        # ConfigMap volumes mount every key as a symlink (via the
                        # ..data indirection, for atomic updates) rather than a
                        # plain regular file. AutoKuma's file source walks
                        # static_monitors with WalkDir::follow_links(false) by
                        # default and only keeps entries where is_file() is true,
                        # so it silently sees zero monitors here otherwise -- no
                        # error, no log line, just nothing to sync, ever.
                        name = "AUTOKUMA__FILES__FOLLOW_SYMLINKS";
                        value = "true";
                      }
                      {
                        name = "AUTOKUMA__ON_DELETE";
                        value = "delete";
                      }
                      {
                        name = "SSL_CERT_FILE";
                        value = "/nix/var/result/etc/ssl/certs/ca-bundle.crt";
                      }
                    ]
                    ++ optionals (cfg.kuma.username != "" && cfg.kuma.password != "") [
                      {
                        name = "AUTOKUMA__KUMA__USERNAME";
                        valueFrom.secretKeyRef = {
                          name = creds-secret;
                          key = "USERNAME";
                        };
                      }
                      {
                        name = "AUTOKUMA__KUMA__PASSWORD";
                        valueFrom.secretKeyRef = {
                          name = creds-secret;
                          key = "PASSWORD";
                        };
                      }
                    ];
                    # No liveness/readiness probe: the /health HTTP endpoint (and its
                    # AUTOKUMA__HEALTHCHECK_PORT config key) were only added upstream in
                    # v2.1.0-rc.2 (github.com/BigBoot/AutoKuma). nixpkgs' pkgs.autokuma is
                    # still on v2.0.0, whose binary has no such server — a probe against any
                    # port here would never succeed and would just crash-loop the container
                    # (this is what happened before: a tcpSocket probe on 8090 killed it
                    # every ~3.5min since nothing was ever listening there). Revisit once
                    # nixpkgs bumps past v2.1.0 stable.
                    volumeMounts = [
                      {
                        name = "nix";
                        mountPath = "/nix";
                        subPath = "nix";
                      }
                      {
                        name = "monitors";
                        mountPath = "/data/monitors";
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
                  {
                    name = "monitors";
                    configMap.name = monitors-configmap;
                  }
                ];
              };
            };
          };
        };
      };
}
