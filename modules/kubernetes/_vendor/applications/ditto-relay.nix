{ ... }:
{
  flake.nixidyApps.ditto-relay =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      name = "ditto-relay";
      labels = {
        "app.kubernetes.io/name" = name;
      };

      esName = "${name}-opensearch";
      esLabels = {
        "app.kubernetes.io/name" = esName;
      };

      nsecSecret = "ditto-relay-nsec";

      # ── Runtime ──────────────────────────────────────────────────────────────
      # gitlab.com/soapbox-pub/ditto-relay ships no upstream Nix package (or
      # even a maintained Docker image tag) -- it's a bun/TypeScript project
      # with no build step (bun runs the sources directly), packaged here as
      # the `ditto-relay-bundle` flake output (modules/pkgs/ditto-relay.nix).
      # nix-csi fetches this repo's own flake by GitHub reference for it, same
      # as applications/nostrarchives.nix and applications/duck1123/default.nix
      # -- meaning a bump of the pinned `rev` in modules/pkgs/ditto-relay.nix
      # needs pushing before nix-csi picks it up.
      nixExpr = ''
        (builtins.getFlake "github:duck1123/k3s-fleetops").packages.x86_64-linux.ditto-relay-bundle
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

        # No k8s Ingress -- reached over a Cloudflare Tunnel public hostname
        # (see applications/cloudflared.nix), same as duck1123. Add a "Public
        # Hostname" route in the Cloudflare Zero Trust dashboard for
        # relay.duck1123.com pointed at
        # ditto-relay.ditto-relay.svc.cluster.local:13131 -- no repo change
        # needed for that step.

        # Shape only -- no volumeHandle here, that's environment-specific (see
        # env/dev/ditto-relay.nix and docs/pinned-volumes.md).
        volumes = cfg: {
          opensearch-data.size = "20Gi";
        };

        extraOptions = {
          relayUrl = mkOption {
            description = mdDoc "Public WebSocket URL the relay advertises to clients (RELAY_URL), e.g. wss://relay.duck1123.com/";
            type = types.str;
            default = "";
          };

          nsec = mkOption {
            description = mdDoc ''
              The relay's own Nostr identity (NOSTR_NSEC) -- used to sign
              internal trend/NIP-85 events. Stored in secrets.enc.yaml as
              `ditto-relay.nsec` and wired in via env/dev/ditto-relay.nix.
            '';
            type = types.str;
            default = "";
          };

          logLevel = mkOption {
            description = mdDoc "LOG_LEVEL: debug | info | warn | error.";
            type = types.str;
            default = "info";
          };

          ipHeader = mkOption {
            description = mdDoc "HTTP header carrying the real client IP behind the Cloudflare Tunnel (IP_HEADER). Empty = use the socket address.";
            type = types.str;
            default = "CF-Connecting-IP";
          };

          opensearch = {
            javaOpts = mkOption {
              description = mdDoc "OPENSEARCH_JAVA_OPTS for the bundled single-node OpenSearch instance.";
              type = types.str;
              default = "-Xms1g -Xmx1g";
            };
          };
        };

        sopsSecrets =
          cfg:
          optionalAttrs (cfg.nsec != "") {
            ${nsecSecret}.NOSTR_NSEC = cfg.nsec;
          };

        extraResources =
          cfg:
          let
            port = 13131;
          in
          {
            deployments.${name}.spec = {
              selector.matchLabels = labels;
              template = {
                metadata.labels = labels;
                spec = {
                  containers = [
                    {
                      inherit name;
                      image = "ghcr.io/lillecarl/nix-csi/scratch:1.0.1";
                      command = [ "ditto-relay" ];
                      env = [
                        {
                          name = "PORT";
                          value = toString port;
                        }
                        {
                          name = "RELAY_URL";
                          value = cfg.relayUrl;
                        }
                        {
                          name = "OPENSEARCH_NODE";
                          value = "http://${esName}.${cfg.namespace}:9200";
                        }
                        {
                          name = "LOG_LEVEL";
                          value = cfg.logLevel;
                        }
                      ]
                      ++ (lib.optionals (cfg.ipHeader != "") [
                        {
                          name = "IP_HEADER";
                          value = cfg.ipHeader;
                        }
                      ])
                      ++ (lib.optionals (cfg.nsec != "") [
                        {
                          name = "NOSTR_NSEC";
                          valueFrom.secretKeyRef = {
                            name = nsecSecret;
                            key = "NOSTR_NSEC";
                          };
                        }
                      ]);
                      ports = [
                        {
                          containerPort = port;
                          name = "http";
                          protocol = "TCP";
                        }
                      ];
                      readinessProbe = {
                        httpGet = {
                          path = "/";
                          port = port;
                        };
                        initialDelaySeconds = 10;
                        periodSeconds = 10;
                        timeoutSeconds = 5;
                        failureThreshold = 3;
                      };
                      livenessProbe = {
                        httpGet = {
                          path = "/";
                          port = port;
                        };
                        initialDelaySeconds = 30;
                        periodSeconds = 30;
                        timeoutSeconds = 5;
                        failureThreshold = 3;
                      };
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

            # No Ingress: the public route (relay.duck1123.com) is a
            # Cloudflare Tunnel Public Hostname pointed at this Service's
            # cluster-internal DNS name (ditto-relay.ditto-relay.svc.cluster.local:13131),
            # configured in the Cloudflare dashboard against the
            # `cloudflared` app's existing tunnel.
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

            # Bundled single-node OpenSearch (required by ditto-relay, not
            # otherwise deployed in this cluster) -- same pattern as
            # applications/tube-archivist.nix's bundled Elasticsearch:
            # security plugin disabled since it's only reachable inside the
            # cluster, and a chown initContainer since the data PVC may start
            # root-owned while the image runs as uid 1000.
            deployments.${esName}.spec = {
              replicas = 1;
              selector.matchLabels = esLabels;
              template = {
                metadata.labels = esLabels;
                spec = {
                  initContainers = [
                    {
                      name = "fix-data-permissions";
                      image = "busybox:latest";
                      imagePullPolicy = "IfNotPresent";
                      command = [
                        "sh"
                        "-c"
                        "chown -R 1000:1000 /usr/share/opensearch/data"
                      ];
                      securityContext.runAsUser = 0;
                      volumeMounts = [
                        {
                          name = "opensearch-data";
                          mountPath = "/usr/share/opensearch/data";
                        }
                      ];
                    }
                  ];
                  containers = [
                    {
                      name = esName;
                      image = "opensearchproject/opensearch:2.19.6";
                      imagePullPolicy = "IfNotPresent";
                      env = [
                        {
                          name = "discovery.type";
                          value = "single-node";
                        }
                        {
                          name = "DISABLE_SECURITY_PLUGIN";
                          value = "true";
                        }
                        {
                          name = "DISABLE_INSTALL_DEMO_CONFIG";
                          value = "true";
                        }
                        {
                          name = "OPENSEARCH_JAVA_OPTS";
                          value = cfg.opensearch.javaOpts;
                        }
                      ];
                      ports = [
                        {
                          containerPort = 9200;
                          name = "http";
                          protocol = "TCP";
                        }
                      ];
                      volumeMounts = [
                        {
                          name = "opensearch-data";
                          mountPath = "/usr/share/opensearch/data";
                        }
                      ];
                    }
                  ];
                  volumes = [
                    cfg.volumes.opensearch-data.volume
                  ];
                };
              };
            };

            services.${esName}.spec = {
              selector = esLabels;
              ports = [
                {
                  name = "http";
                  port = 9200;
                  targetPort = 9200;
                  protocol = "TCP";
                }
              ];
            };
          };
      };
}
