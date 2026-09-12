{ ... }:
{
  flake.nixidyApps.kapowarr =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
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
        name = "kapowarr";
        uses-ingress = true;

        # Shape only -- no volumeHandle here, that's environment-specific (see
        # env/dev/kapowarr.nix and docs/pinned-volumes.md).
        volumes = cfg: {
          config = {
            size = "5Gi";
            volumeAttributes.backupTargetName = "default";
          };
        };

        extraOptions = {
          image = mkOption {
            description = mdDoc "The docker image";
            type = types.str;
            default = "mrcas/kapowarr:latest";
          };

          service.port = mkOption {
            description = mdDoc "The service port";
            type = types.int;
            default = 5656;
          };

          vpn = {
            enable = mkOption {
              description = mdDoc "Enable VPN routing through shared gluetun service";
              type = types.bool;
              default = true;
            };

            sharedGluetunService = mkOption {
              description = mdDoc "Service name for shared gluetun (e.g., gluetun.gluetun)";
              type = types.str;
              default = "gluetun.gluetun";
            };
          };

          nfs = {
            enable = mkOption {
              description = mdDoc "Enable NFS for downloads volume";
              type = types.bool;
              default = false;
            };

            server = mkOption {
              description = mdDoc "NFS server hostname/IP";
              type = types.str;
              default = "nasnix";
            };

            path = mkOption {
              description = mdDoc "NFS server base path, downloads subfolder is read from <path>/Downloads/comics";
              type = types.str;
              default = "/mnt/media";
            };

            library = {
              enable = mkOption {
                description = mdDoc "Mount NFS path for the comics library (root folder)";
                type = types.bool;
                default = false;
              };

              path = mkOption {
                description = mdDoc "NFS path for the comics library (e.g. /volume1/Books)";
                type = types.str;
                default = "/Books";
              };
            };
          };

          pgid = mkOption {
            description = mdDoc "The group ID";
            type = types.int;
            default = 1000;
          };

          puid = mkOption {
            description = mdDoc "The user ID";
            type = types.int;
            default = 1000;
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
        };

        extraResources = cfg: {
          deployments = {
            ${name} = {
              metadata.labels = {
                "app.kubernetes.io/instance" = name;
                "app.kubernetes.io/name" = name;
                "app.kubernetes.io/version" = "latest";
              };

              spec = {
                replicas = cfg.replicas;
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
                    automountServiceAccountToken = true;
                    serviceAccountName = "default";

                    initContainers = lib.optionalAttrs cfg.vpn.enable (
                      self.lib.waitForGluetun { inherit lib; } cfg.vpn.sharedGluetunService
                    );

                    containers = [
                      {
                        inherit name;
                        image = cfg.image;
                        imagePullPolicy = "IfNotPresent";
                        env = [
                          {
                            name = "PGID";
                            value = "${toString cfg.pgid}";
                          }
                          {
                            name = "PUID";
                            value = "${toString cfg.puid}";
                          }
                          {
                            name = "TZ";
                            value = cfg.tz;
                          }
                        ]
                        ++ (lib.optionals cfg.vpn.enable [
                          # Configure Kapowarr to use shared gluetun's HTTP proxy
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
                            value = "localhost,127.0.0.1,.svc,.svc.cluster.local,sabnzbd.sabnzbd,sabnzbd.sabnzbd.svc.cluster.local";
                          }
                        ]);
                        ports = [
                          {
                            containerPort = cfg.service.port;
                            name = "http";
                            protocol = "TCP";
                          }
                        ];
                        readinessProbe = lib.mkIf cfg.useProbes {
                          tcpSocket.port = cfg.service.port;
                          initialDelaySeconds = 30;
                          periodSeconds = 10;
                          timeoutSeconds = 5;
                          successThreshold = 1;
                          failureThreshold = 3;
                        };
                        livenessProbe = lib.mkIf cfg.useProbes {
                          tcpSocket.port = cfg.service.port;
                          initialDelaySeconds = 60;
                          periodSeconds = 30;
                          timeoutSeconds = 5;
                          successThreshold = 1;
                          failureThreshold = 3;
                        };
                        volumeMounts = [
                          {
                            mountPath = "/app/db";
                            name = "config";
                          }
                          {
                            mountPath = "/app/temp_downloads";
                            name = "downloads";
                          }
                        ]
                        ++ (lib.optionals (cfg.nfs.enable && cfg.nfs.library.enable) [
                          {
                            mountPath = "/comics";
                            name = "library";
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
                    ++ (lib.optionals (cfg.nfs.enable && cfg.nfs.library.enable) [
                      {
                        name = "library";
                        persistentVolumeClaim.claimName = "${name}-${name}-library";
                      }
                    ]);
                  };
                };
              };
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
          // (lib.optionalAttrs (cfg.nfs.enable && cfg.nfs.library.enable) {
            "${name}-${name}-library".spec = {
              accessModes = [ "ReadWriteMany" ];
              resources.requests.storage = "1Gi";
              storageClassName = "";
              volumeName = "${name}-${name}-library-nfs";
            };
          });

          services.${name}.spec = {
            ports = [
              {
                name = "http";
                port = cfg.service.port;
                protocol = "TCP";
                targetPort = "http";
              }
            ];

            selector = {
              "app.kubernetes.io/instance" = name;
              "app.kubernetes.io/name" = name;
            };

            type = "ClusterIP";
          };

          # Create NFS PersistentVolumes for downloads/comics and the comics library when NFS is enabled
          persistentVolumes =
            lib.optionalAttrs (cfg.nfs.enable) {
              "${name}-${name}-downloads-nfs" = {
                apiVersion = "v1";
                kind = "PersistentVolume";
                metadata = {
                  name = "${name}-${name}-downloads-nfs";
                };
                spec = {
                  capacity = {
                    storage = "1Ti";
                  };
                  accessModes = [ "ReadWriteMany" ];
                  mountOptions = [
                    "nolock"
                    "noexec"
                    "soft"
                    "timeo=30"
                  ];
                  nfs = {
                    server = cfg.nfs.server;
                    path = "${cfg.nfs.path}/Downloads/comics";
                  };
                  persistentVolumeReclaimPolicy = "Retain";
                };
              };
            }
            // (lib.optionalAttrs (cfg.nfs.enable && cfg.nfs.library.enable) {
              "${name}-${name}-library-nfs" = {
                apiVersion = "v1";
                kind = "PersistentVolume";
                metadata = {
                  name = "${name}-${name}-library-nfs";
                };
                spec = {
                  capacity = {
                    storage = "1Ti";
                  };
                  accessModes = [ "ReadWriteMany" ];
                  mountOptions = [
                    "nolock"
                    "noexec"
                    "soft"
                    "timeo=30"
                  ];
                  nfs = {
                    server = cfg.nfs.server;
                    path = cfg.nfs.library.path;
                  };
                  persistentVolumeReclaimPolicy = "Retain";
                };
              };
            });
          # config is a pinned volume -- see the `pinnedVolumes` attrset above;
          # its PersistentVolume/PersistentVolumeClaim resources are merged in
          # automatically by mkArgoApp.
        };
      };
}
