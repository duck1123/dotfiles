{ ... }:
{
  flake.nixidyApps.xysat =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      name = "xysat";
      setup-secret = "${name}-setup";
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

        # The bootstrap URL (http://xyops.xyops:5522/api/app/satellite/config?t=API_KEY)
        # embeds a non-expiring API Key generated from the xyOps UI (Settings ->
        # API Keys, "add_servers" privilege only) -- there's no way to mint this
        # ahead of time from Nix, since it's issued by the running conductor.
        # See env/dev/xysat.nix for how it's wired from secrets.xysat.setupApiKey.
        sopsSecrets =
          cfg:
          optionalAttrs (cfg.setupUrl != "") {
            ${setup-secret}.SETUP_URL = cfg.setupUrl;
          };

        extraOptions = {
          image = mkOption {
            description = mdDoc "The xySat container image";
            type = types.str;
            default = "ghcr.io/pixlcore/xysat:latest";
          };

          setupUrl = mkOption {
            description = mdDoc ''
              Full bootstrap URL including a non-expiring API Key, e.g.
              http://xyops.xyops:5522/api/app/satellite/config?t=YOUR_API_KEY
              Only consulted on first launch per node -- once /etc/xysat/config.json
              exists on a node (persisted via hostConfigDir), it's skipped on restart.
            '';
            type = types.str;
            default = "";
          };

          hostConfigDir = mkOption {
            description = mdDoc "Host directory (bind-mounted per node) holding this node's persistent xySat config/server identity";
            type = types.str;
            default = "/var/lib/xysat";
          };

          nixExpr = mkOption {
            description = mdDoc ''
              Nix expression for nix-csi to build and mount at /nix (subPath
              "nix") inside the xysat container, exposing
              /nix/var/result/bin -- this gives jobs the satellite runs
              access to whatever the expression's package set includes.
              PATH is set to include /nix/var/result/bin whenever this is
              non-empty. Empty string (default) disables the nix-csi volume
              entirely -- the pixlcore/xysat image is unaffected.
            '';
            type = types.str;
            default = "";
          };
        };

        extraResources =
          cfg:
          let
            labels = {
              "app.kubernetes.io/instance" = name;
              "app.kubernetes.io/name" = name;
            };
          in
          {
            daemonSets.${name} = {
              metadata.labels = labels;

              spec = {
                selector.matchLabels = labels;

                template = {
                  metadata.labels = labels;

                  spec = {
                    # Real host visibility (CPU/mem/process/network), not just this
                    # container's own cgroup -- and lets cluster DNS still resolve
                    # (needed since hostNetwork changes the default DNS policy).
                    hostNetwork = true;
                    hostPID = true;
                    dnsPolicy = "ClusterFirstWithHostNet";

                    # Run on every node, including the nasnix control-plane node.
                    tolerations = [
                      { operator = "Exists"; }
                    ];

                    containers = [
                      {
                        inherit name;
                        image = cfg.image;
                        imagePullPolicy = "IfNotPresent";

                        env = [
                          {
                            name = "XYSAT_config_file";
                            value = "/etc/xysat/config.json";
                          }
                        ]
                        ++ optionals (cfg.setupUrl != "") [
                          {
                            name = "XYOPS_setup";
                            valueFrom.secretKeyRef = {
                              name = setup-secret;
                              key = "SETUP_URL";
                            };
                          }
                        ]
                        ++ optionals (cfg.nixExpr != "") [
                          {
                            name = "PATH";
                            value = "/nix/var/result/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin";
                          }
                          {
                            # extra-* appends to (rather than replaces) nix's built-in
                            # defaults, so cache.nixos.org stays in the substituter list
                            # alongside the self-hosted Attic cache (see
                            # docs/nix-csi-and-binary-cache.md).
                            #
                            # `store = local?root=...` points nix's actual store (db +
                            # /nix/store writes) at the writable scratch emptyDir below,
                            # completely separate from the read-only nix-csi mount at
                            # /nix (which only supplies the base toolset binaries on
                            # PATH). This is what lets `nix build`/`nix run`/`nix shell`
                            # fetch or build anything, not just what's baked into
                            # nixExpr -- ephemeral by design, wiped on pod restart.
                            #
                            # sandbox = false + build-users-group = "" are required to
                            # build as root with no nixbld user/group present in the
                            # pixlcore/xysat image, and no CAP_SYS_ADMIN for the usual
                            # sandbox namespaces -- builds run unsandboxed as a result
                            # (acceptable here since these are ephemeral automation
                            # workers, not a shared multi-tenant store).
                            name = "NIX_CONFIG";
                            value = ''
                              experimental-features = nix-command flakes
                              extra-substituters = https://attic.home.kronkltd.net/nixos
                              extra-trusted-public-keys = nixos:6s8iAyKEnH2z4spigUdDmt1VwiAwrvPA9vQNUd9if1k=
                              store = local?root=/var/lib/nix-scratch
                              sandbox = false
                              build-users-group =
                            '';
                          }
                        ];

                        volumeMounts = [
                          {
                            mountPath = "/etc/xysat";
                            name = "conf";
                          }
                        ]
                        ++ optionals (cfg.nixExpr != "") [
                          {
                            mountPath = "/nix";
                            name = "nix";
                            subPath = "nix";
                          }
                          {
                            mountPath = "/var/lib/nix-scratch";
                            name = "nix-scratch";
                          }
                        ];
                      }
                    ];

                    volumes = [
                      {
                        name = "conf";
                        hostPath = {
                          path = cfg.hostConfigDir;
                          type = "DirectoryOrCreate";
                        };
                      }
                    ]
                    ++ optionals (cfg.nixExpr != "") [
                      {
                        name = "nix";
                        csi = {
                          driver = "nix.csi.store";
                          volumeAttributes.nixExpr = cfg.nixExpr;
                        };
                      }
                      {
                        # Writable, node-local, wiped on pod restart -- see the
                        # NIX_CONFIG `store` setting above.
                        name = "nix-scratch";
                        emptyDir = { };
                      }
                    ];
                  };
                };
              };
            };
          };
      };
}
