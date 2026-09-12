{ ... }:
{
  flake.nixidyApps.lidarr =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      password-secret = "lidarr-database-password";
      cfg = config.services.lidarr;
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
        name = "lidarr";
        uses-ingress = true;
        uses-database = true;

        # Shape only -- no volumeHandle here, that's environment-specific (see
        # env/dev/lidarr.nix and docs/pinned-volumes.md).
        volumes = cfg: {
          config.size = "10Gi";
        };

        extraOptions = {
          image = mkOption {
            description = mdDoc "The docker image";
            type = types.str;
            default = "linuxserver/lidarr:latest";
          };

          service.port = mkOption {
            description = mdDoc "The service port";
            type = types.int;
            default = 8686;
          };

          apiKey = mkOption {
            description = mdDoc ''
              Lidarr API key (Settings -> General -> Security). Stored in
              secrets.enc.yaml as `lidarr.key` and wired in via
              `env/dev/lidarr.nix`. Only powers the auto-added homepage
              dashboard widget below -- never injected into the lidarr
              container itself.
            '';
            type = types.str;
            default = "";
          };

          # Auto-add a Lidarr widget to this app's homepage dashboard tile once
          # an API key is configured -- see applications/immich.nix for the same
          # pattern with more detail. Set
          # `services.homepage.widgetSecrets.LIDARR_API_KEY` from
          # `config.services.lidarr.apiKey` in env/dev/homepage.nix.
          homepage.extraSettings = mkOption {
            default = lib.optionalAttrs (cfg.apiKey != "") {
              widget = {
                type = "lidarr";
                url = "http://${name}.${cfg.namespace}:${toString cfg.service.port}";
                key = "{{HOMEPAGE_VAR_LIDARR_API_KEY}}";
              };
            };
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
              description = mdDoc "NFS server path";
              type = types.str;
              default = "/mnt/media";
            };

            slskdDownloads = {
              enable = mkOption {
                description = mdDoc "Mount NFS path for Slskd/Soularr downloads so Lidarr can import them";
                type = types.bool;
                default = false;
              };

              path = mkOption {
                description = mdDoc "NFS path for Slskd downloads (e.g. /volume1/slskd_downloads)";
                type = types.str;
                default = "/slskd_downloads";
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

        sopsSecrets =
          cfg:
          lib.optionalAttrs (cfg.database.enable && cfg.database.password != "") {
            ${password-secret} = {
              password = cfg.database.password;
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
                    securityContext = {
                      fsGroup = cfg.pgid;
                    };
                    serviceAccountName = "default";
                    initContainers = [
                      {
                        name = "fix-config-permissions";
                        image = "busybox:latest";
                        imagePullPolicy = "IfNotPresent";
                        command = [
                          "chown"
                          "-R"
                          "${toString cfg.puid}:${toString cfg.pgid}"
                          "/config"
                        ];
                        securityContext.runAsUser = 0;
                        volumeMounts = [
                          {
                            mountPath = "/config";
                            name = "config";
                          }
                        ];
                      }
                    ]
                    ++ (lib.optionals cfg.vpn.enable (
                      self.lib.waitForGluetun { inherit lib; } cfg.vpn.sharedGluetunService
                    ));
                    containers = [
                      {
                        inherit name;
                        image = cfg.image;
                        imagePullPolicy = "IfNotPresent";
                        # Run as root so wrapper can chown /config; image /init then drops to PUID:PGID
                        securityContext.runAsUser = 0;
                        # Chown /config in same container as app so mount is identical (fixes NFS/storage permission issues)
                        command = [
                          "sh"
                          "-c"
                          "chown -R ${toString cfg.puid}:${toString cfg.pgid} /config && exec /init"
                        ];
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
                        ++ (lib.optionals cfg.database.enable [
                          {
                            name = "LIDARR__POSTGRES__HOST";
                            value = cfg.database.host;
                          }
                          {
                            name = "LIDARR__POSTGRES__PORT";
                            value = toString cfg.database.port;
                          }
                          {
                            name = "LIDARR__POSTGRES__MAINDB";
                            value = cfg.database.name;
                          }
                          {
                            name = "LIDARR__POSTGRES__LOGDB";
                            value =
                              if lib.hasSuffix "-main" cfg.database.name then
                                lib.removeSuffix "-main" cfg.database.name + "-log"
                              else
                                "${cfg.database.name}-log";
                          }
                          {
                            name = "LIDARR__POSTGRES__USER";
                            value = cfg.database.username;
                          }
                          (
                            if cfg.database.password != "" then
                              {
                                name = "LIDARR__POSTGRES__PASSWORD";
                                valueFrom = {
                                  secretKeyRef = {
                                    name = password-secret;
                                    key = "password";
                                  };
                                };
                              }
                            else
                              {
                                name = "LIDARR__POSTGRES__PASSWORD";
                                value = "";
                              }
                          )
                        ])
                        ++ (lib.optionals cfg.vpn.enable [
                          # Configure Lidarr to use shared gluetun's HTTP proxy
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
                          # Use exec probe to test from inside container (works with IPv6 binding)
                          exec = {
                            command = [
                              "sh"
                              "-c"
                              "wget -q -O- --timeout=2 http://127.0.0.1:${toString cfg.service.port}/ || wget -q -O- --timeout=2 http://[::1]:${toString cfg.service.port}/ || exit 1"
                            ];
                          };
                          initialDelaySeconds = 60;
                          periodSeconds = 10;
                          timeoutSeconds = 5;
                          successThreshold = 1;
                          failureThreshold = 5;
                        };
                        livenessProbe = lib.mkIf cfg.useProbes {
                          # Use exec probe to test from inside container (works with IPv6 binding)
                          exec = {
                            command = [
                              "sh"
                              "-c"
                              "wget -q -O- --timeout=2 http://127.0.0.1:${toString cfg.service.port}/ || wget -q -O- --timeout=2 http://[::1]:${toString cfg.service.port}/ || exit 1"
                            ];
                          };
                          initialDelaySeconds = 90;
                          periodSeconds = 30;
                          timeoutSeconds = 5;
                          successThreshold = 1;
                          failureThreshold = 3;
                        };
                        volumeMounts = [
                          {
                            mountPath = "/config";
                            name = "config";
                          }
                          {
                            mountPath = "/downloads";
                            name = "downloads";
                          }
                          {
                            mountPath = "/music";
                            name = "music";
                          }
                        ]
                        ++ (lib.optionals (cfg.nfs.enable && cfg.nfs.slskdDownloads.enable) [
                          {
                            mountPath = "/downloads/slskd_downloads";
                            name = "slskd-downloads";
                          }
                        ]);
                      }
                    ];
                    volumes = [
                      cfg.volumes.config.volume
                    ]
                    ++ (lib.optionals (cfg.database.enable && cfg.database.password != "") [
                      {
                        name = password-secret;
                        secret.secretName = password-secret;
                      }
                    ])
                    ++ [
                      {
                        name = "downloads";
                        persistentVolumeClaim.claimName = "${name}-${name}-downloads";
                      }
                      {
                        name = "music";
                        persistentVolumeClaim.claimName = "${name}-${name}-music";
                      }
                    ]
                    ++ (lib.optionals (cfg.nfs.enable && cfg.nfs.slskdDownloads.enable) [
                      {
                        name = "slskd-downloads";
                        persistentVolumeClaim.claimName = "${name}-${name}-slskd-downloads";
                      }
                    ]);
                  };
                };
              };
            };
          };

          ingresses.${name} = with cfg.ingress; {
            metadata.annotations."cert-manager.io/cluster-issuer" = clusterIssuer;
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
                  secretName = "${name}-tls";
                }
              ];
            };
          };

          persistentVolumes = lib.optionalAttrs (cfg.nfs.enable) (
            {
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
                    path = "${cfg.nfs.path}/Downloads";
                  };
                  persistentVolumeReclaimPolicy = "Retain";
                };
              };
              "${name}-${name}-music-nfs" = {
                apiVersion = "v1";
                metadata.name = "${name}-${name}-music-nfs";
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
                    path = "${cfg.nfs.path}/Music";
                  };
                  persistentVolumeReclaimPolicy = "Retain";
                };
              };
            }
            // (lib.optionalAttrs cfg.nfs.slskdDownloads.enable {
              "${name}-${name}-slskd-downloads-nfs" = {
                apiVersion = "v1";
                metadata.name = "${name}-${name}-slskd-downloads-nfs";
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
                    path = cfg.nfs.slskdDownloads.path;
                  };
                  persistentVolumeReclaimPolicy = "Retain";
                };
              };
            })
          );

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
            "${name}-${name}-music".spec =
              if cfg.nfs.enable then
                {
                  accessModes = [ "ReadWriteMany" ];
                  resources.requests.storage = "1Gi";
                  storageClassName = "";
                  volumeName = "${name}-${name}-music-nfs";
                }
              else
                {
                  inherit (cfg) storageClassName;
                  accessModes = [ "ReadWriteOnce" ];
                  resources.requests.storage = "100Gi";
                };
          }
          // (lib.optionalAttrs (cfg.nfs.enable && cfg.nfs.slskdDownloads.enable) {
            "${name}-${name}-slskd-downloads".spec = {
              accessModes = [ "ReadWriteMany" ];
              resources.requests.storage = "1Gi";
              storageClassName = "";
              volumeName = "${name}-${name}-slskd-downloads-nfs";
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

        };
      };
}
