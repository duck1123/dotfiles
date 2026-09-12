{ ... }:
{
  flake.nixidyApps.slskd =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      web-auth-secret = "slskd-web-credentials";
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
      rec {
        name = "slskd";
        uses-ingress = true;

        # Shape only -- no volumeHandle here, that's environment-specific (see
        # env/dev/slskd.nix and docs/pinned-volumes.md).
        volumes = cfg: {
          config = {
            # Real storageClassName (not the "" default) so Longhorn's resize
            # webhook treats this as expandable -- it was full, see
            # modules/lib/mkPinnedVolume.nix for why "" blocks resize.
            storageClassName = "longhorn";
            size = "40Gi";
          };
        };

        sopsSecrets =
          cfg:
          lib.optionalAttrs (cfg.webAuth.username != "" && cfg.webAuth.password != "") {
            ${web-auth-secret} = {
              username = cfg.webAuth.username;
              password = cfg.webAuth.password;
            }
            // lib.optionalAttrs (cfg.apiKey != "") {
              apiKey = cfg.apiKey;
            };
          };

        extraOptions = {
          image = mkOption {
            description = mdDoc "Slskd Docker image (Soulseek client; used by Soularr)";
            type = types.str;
            default = "slskd/slskd:latest";
          };

          service.port = mkOption {
            description = mdDoc "Web UI / API port";
            type = types.int;
            default = 5030;
          };

          replicas = mkOption {
            description = mdDoc "Number of replicas";
            type = types.int;
            default = 1;
          };

          useProbes = mkOption {
            description = mdDoc "Enable readiness and liveness probes";
            type = types.bool;
            default = true;
          };

          webAuth = {
            username = mkOption {
              description = mdDoc "Web UI login username (stored in secret); set from secrets.slskd.username";
              type = types.str;
              default = "";
            };

            password = mkOption {
              description = mdDoc "Web UI login password (stored in secret); set from secrets.slskd.password";
              type = types.str;
              default = "";
            };
          };

          apiKey = mkOption {
            description = mdDoc "API key for slskd (stored in secret); set from secrets.slskd.apiKey; used by Soularr etc.";
            type = types.str;
            default = "";
          };

          vpn = {
            enable = mkOption {
              description = mdDoc "Route traffic through shared Gluetun VPN (HTTP proxy)";
              type = types.bool;
              default = false;
            };

            sharedGluetunService = mkOption {
              description = mdDoc "Service name for shared Gluetun (e.g. gluetun.gluetun)";
              type = types.str;
              default = "gluetun.gluetun";
            };
          };

          nfs = {
            enable = mkOption {
              description = mdDoc "Use NFS for download directory (same path as Soularr/Lidarr)";
              type = types.bool;
              default = false;
            };

            server = mkOption {
              description = mdDoc "NFS server hostname/IP";
              type = types.str;
              default = "nasnix";
            };

            path = mkOption {
              description = mdDoc "NFS path for downloads (e.g. /volume1/slskd_downloads)";
              type = types.str;
              default = "/mnt/media/slskd_downloads";
            };
          };

          shares = {
            enable = mkOption {
              description = mdDoc "Mount an NFS path as shared content (e.g. music collection) for Soulseek sharing; add the mount path in Slskd Web UI → Shares";
              type = types.bool;
              default = false;
            };

            server = mkOption {
              description = mdDoc "NFS server for shares (e.g. NAS hostname)";
              type = types.str;
              default = "nasnix";
            };

            path = mkOption {
              description = mdDoc "NFS path to music/library to share (e.g. /volume1/Music)";
              type = types.str;
              default = "/mnt/media";
            };
          };
        };

        extraResources = cfg: {
          deployments.${name} = {
            metadata.labels = {
              "app.kubernetes.io/instance" = name;
              "app.kubernetes.io/name" = name;
            };

            spec = {
              replicas = cfg.replicas;
              strategy.type = "Recreate";
              selector.matchLabels = {
                "app.kubernetes.io/instance" = name;
                "app.kubernetes.io/name" = name;
              };

              template = {
                metadata.labels = {
                  "app.kubernetes.io/instance" = name;
                  "app.kubernetes.io/name" = name;
                };

                spec = {
                  securityContext.fsGroup = 1000;
                  serviceAccountName = "default";
                  initContainers = lib.optionals cfg.vpn.enable (
                    self.lib.waitForGluetun { inherit lib; } cfg.vpn.sharedGluetunService
                  );
                  containers = [
                    {
                      inherit name;
                      image = cfg.image;
                      imagePullPolicy = "IfNotPresent";
                      env = [
                        {
                          name = "TZ";
                          value = cfg.tz;
                        }
                        {
                          name = "SLSKD_REMOTE_CONFIGURATION";
                          value = "true";
                        }
                        {
                          name = "SLSKD_SHARED_DIR";
                          value = "/shares";
                        }
                      ]
                      ++ (lib.optionals (cfg.webAuth.username != "" && cfg.webAuth.password != "") [
                        {
                          name = "SLSKD_SLSK_USERNAME";
                          valueFrom.secretKeyRef = {
                            name = web-auth-secret;
                            key = "username";
                          };
                        }
                        {
                          name = "SLSKD_SLSK_PASSWORD";
                          valueFrom.secretKeyRef = {
                            name = web-auth-secret;
                            key = "password";
                          };
                        }
                        {
                          name = "SLSKD_USERNAME";
                          valueFrom.secretKeyRef = {
                            name = web-auth-secret;
                            key = "username";
                          };
                        }
                        {
                          name = "SLSKD_PASSWORD";
                          valueFrom.secretKeyRef = {
                            name = web-auth-secret;
                            key = "password";
                          };
                        }
                      ])
                      ++ (lib.optionals (cfg.webAuth.username != "" && cfg.webAuth.password != "" && cfg.apiKey != "") [
                        {
                          name = "SLSKD_API_KEY";
                          valueFrom.secretKeyRef = {
                            name = web-auth-secret;
                            key = "apiKey";
                          };
                        }
                      ])
                      ++ (lib.optionals cfg.vpn.enable [
                        {
                          name = "HTTP_PROXY";
                          value = "http://${cfg.vpn.sharedGluetunService}:8888";
                        }
                        {
                          name = "HTTPS_PROXY";
                          value = "http://${cfg.vpn.sharedGluetunService}:8888";
                        }
                        {
                          name = "NO_PROXY";
                          value = "localhost,127.0.0.1,.svc,.svc.cluster.local,soularr.soularr,lidarr.lidarr";
                        }
                      ]);
                      ports = [
                        {
                          name = "http";
                          containerPort = cfg.service.port;
                          protocol = "TCP";
                        }
                        {
                          name = "https";
                          containerPort = 5031;
                          protocol = "TCP";
                        }
                        {
                          name = "p2p";
                          containerPort = 50300;
                          protocol = "TCP";
                        }
                      ];
                      readinessProbe = lib.mkIf cfg.useProbes {
                        httpGet = {
                          path = "/";
                          port = cfg.service.port;
                          scheme = "HTTP";
                        };
                        initialDelaySeconds = 30;
                        periodSeconds = 10;
                        timeoutSeconds = 5;
                        successThreshold = 1;
                        failureThreshold = 5;
                      };
                      livenessProbe = lib.mkIf cfg.useProbes {
                        httpGet = {
                          path = "/";
                          port = cfg.service.port;
                          scheme = "HTTP";
                        };
                        initialDelaySeconds = 60;
                        periodSeconds = 30;
                        timeoutSeconds = 5;
                        successThreshold = 1;
                        failureThreshold = 3;
                      };
                      volumeMounts = [
                        {
                          mountPath = "/app";
                          name = "config";
                        }
                        {
                          mountPath = "/app/downloads";
                          name = "downloads";
                        }
                      ]
                      ++ (lib.optionals cfg.shares.enable [
                        {
                          mountPath = "/shares";
                          name = "shares";
                        }
                      ]);
                    }
                  ];
                  volumes = [
                    cfg.volumes.config.volume
                    {
                      name = "downloads";
                      persistentVolumeClaim.claimName = "${name}-${name}-downloads";
                    }
                  ]
                  ++ (lib.optionals cfg.shares.enable [
                    {
                      name = "shares";
                      persistentVolumeClaim.claimName = "${name}-${name}-shares";
                    }
                  ]);
                };
              };
            };
          };

          persistentVolumes =
            lib.optionalAttrs cfg.nfs.enable {
              "${name}-${name}-downloads-nfs" = {
                apiVersion = "v1";
                metadata.name = "${name}-${name}-downloads-nfs";
                spec = {
                  accessModes = [ "ReadWriteMany" ];
                  capacity.storage = "1Ti";
                  mountOptions = [
                    "nolock"
                    "noexec"
                    "soft"
                    "timeo=30"
                  ];
                  nfs = {
                    server = cfg.nfs.server;
                    path = cfg.nfs.path;
                  };
                  persistentVolumeReclaimPolicy = "Retain";
                };
              };
            }
            // lib.optionalAttrs cfg.shares.enable {
              "${name}-${name}-shares-nfs" = {
                apiVersion = "v1";
                metadata.name = "${name}-${name}-shares-nfs";
                spec = {
                  accessModes = [ "ReadWriteMany" ];
                  capacity.storage = "1Ti";
                  mountOptions = [
                    "nolock"
                    "noexec"
                    "soft"
                    "timeo=30"
                  ];
                  nfs = {
                    server = cfg.shares.server;
                    path = cfg.shares.path;
                  };
                  persistentVolumeReclaimPolicy = "Retain";
                };
              };
            };

          persistentVolumeClaims = {
            "${name}-${name}-downloads".spec =
              if cfg.nfs.enable then
                {
                  accessModes = [ "ReadWriteMany" ];
                  resources.requests.storage = "1Gi";
                  storageClassName = "";
                  volumeName = "${name}-${name}-downloads-nfs";
                }
              else
                {
                  inherit (cfg) storageClassName;
                  accessModes = [ "ReadWriteOnce" ];
                  resources.requests.storage = "50Gi";
                };
          }
          // lib.optionalAttrs cfg.shares.enable {
            "${name}-${name}-shares".spec = {
              accessModes = [ "ReadWriteMany" ];
              resources.requests.storage = "1Gi";
              storageClassName = "";
              volumeName = "${name}-${name}-shares-nfs";
            };
          };

          ingresses.${name} = with cfg.ingress; {
            metadata.annotations = optionalAttrs (clusterIssuer != "") {
              "cert-manager.io/cluster-issuer" = clusterIssuer;
            };

            spec = {
              inherit ingressClassName;

              rules = [
                {
                  host = domain;

                  http.paths = [
                    {
                      backend.service = {
                        inherit name;
                        port.name = "http";
                      };

                      path = "/";
                      pathType = "ImplementationSpecific";
                    }
                  ];
                }
              ];

              tls = [
                {
                  hosts = [ domain ];
                  secretName = "${domain}-tls";
                }
              ];
            };
          };

          services.${name}.spec = {
            ports = [
              {
                name = "http";
                port = cfg.service.port;
                protocol = "TCP";
                targetPort = "http";
              }
              {
                name = "https";
                port = 5031;
                protocol = "TCP";
                targetPort = "https";
              }
              {
                name = "p2p";
                port = 50300;
                protocol = "TCP";
                targetPort = "p2p";
              }
            ];

            selector = {
              "app.kubernetes.io/instance" = name;
              "app.kubernetes.io/name" = name;
            };

            type = "ClusterIP";
          };
        };
      };
}
