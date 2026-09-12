{ ... }:
{
  flake.nixidyApps.fileflows =
    {
      config,
      lib,
      self,
      ...
    }:
    with lib;
    self.lib.mkArgoApp { inherit config lib self; } rec {
      name = "fileflows";
      uses-ingress = true;

      # Shape only -- no volumeHandle here, that's environment-specific (see
      # env/dev/fileflows.nix and docs/pinned-volumes.md).
      volumes = cfg: {
        config.size = "5Gi";
        temp.size = "50Gi";
      };

      extraOptions = {
        image = mkOption {
          description = mdDoc "The docker image";
          type = types.str;
          default = "revenz/fileflows:latest";
        };

        service.port = mkOption {
          description = mdDoc "The web UI service port";
          type = types.int;
          default = 5000;
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

        nfs = {
          enable = mkOption {
            description = mdDoc "Enable NFS for media volumes (Movies, TV, Videos)";
            type = types.bool;
            default = false;
          };

          server = mkOption {
            description = mdDoc "NFS server hostname/IP";
            type = types.str;
            default = "nasnix";
          };

          path = mkOption {
            description = mdDoc "NFS server base path (shares are mounted as subdirectories)";
            type = types.str;
            default = "/mnt/media";
          };

          enableVideos = mkOption {
            description = mdDoc "Also mount the Videos share alongside Movies and TV";
            type = types.bool;
            default = false;
          };
        };

        enableGPU = mkOption {
          description = mdDoc "Enable GPU for hardware transcoding (mounts /dev/dri for Intel/AMD iGPU)";
          type = types.bool;
          default = false;
        };

        ingress.annotations = mkOption {
          description = mdDoc "Annotations for the Ingress resource";
          type = types.attrsOf types.str;
          default = { };
        };
      };

      extraResources =
        cfg:
        let
          mediaMounts = lib.optionals cfg.nfs.enable (
            [
              {
                mountPath = "/media/Movies";
                name = "media-movies";
              }
              {
                mountPath = "/media/TV";
                name = "media-tv";
              }
            ]
            ++ lib.optionals cfg.nfs.enableVideos [
              {
                mountPath = "/media/Videos";
                name = "media-videos";
              }
            ]
          );

          mediaVolumes = lib.optionals cfg.nfs.enable (
            [
              {
                name = "media-movies";
                persistentVolumeClaim.claimName = "${name}-${name}-media-movies";
              }
              {
                name = "media-tv";
                persistentVolumeClaim.claimName = "${name}-${name}-media-tv";
              }
            ]
            ++ lib.optionals cfg.nfs.enableVideos [
              {
                name = "media-videos";
                persistentVolumeClaim.claimName = "${name}-${name}-media-videos";
              }
            ]
          );

          nfsPVOptions = {
            accessModes = [ "ReadWriteMany" ];
            mountOptions = [
              "nolock"
              "noexec"
              "soft"
              "timeo=30"
            ];
            persistentVolumeReclaimPolicy = "Retain";
            capacity.storage = "1Ti";
          };
        in
        {
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
                      fsGroupChangePolicy = "OnRootMismatch";
                      runAsUser = if cfg.enableGPU then 0 else cfg.puid;
                      runAsGroup = if cfg.enableGPU then 0 else cfg.pgid;
                      supplementalGroups = lib.optionals cfg.enableGPU [ cfg.renderGroupGID ];
                    };

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
                          {
                            name = "FFMPEG_VAAPI";
                            value = if cfg.enableGPU then "1" else "0";
                          }
                        ]
                        ++ (lib.optionals (cfg.libvaDriverName != "") [
                          {
                            name = "LIBVA_DRIVER_NAME";
                            value = cfg.libvaDriverName;
                          }
                        ])
                        ++ (lib.optionals cfg.enableGPU [
                          {
                            name = "XDG_RUNTIME_DIR";
                            value = "";
                          }
                          {
                            name = "LIBVA_DRIVERS_PATH";
                            value = "/usr/lib/x86_64-linux-gnu/dri";
                          }
                          {
                            name = "LIBVA_MESSAGING";
                            value = "1";
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
                          httpGet = {
                            path = "/";
                            port = cfg.service.port;
                          };
                          initialDelaySeconds = 60;
                          periodSeconds = 15;
                          timeoutSeconds = 10;
                          successThreshold = 1;
                          failureThreshold = 6;
                        };

                        livenessProbe = lib.mkIf cfg.useProbes {
                          httpGet = {
                            path = "/";
                            port = cfg.service.port;
                          };
                          initialDelaySeconds = 120;
                          periodSeconds = 30;
                          timeoutSeconds = 10;
                          successThreshold = 1;
                          failureThreshold = 3;
                        };

                        volumeMounts = [
                          {
                            mountPath = "/app/Data";
                            name = "config";
                          }
                          {
                            mountPath = "/temp";
                            name = "temp";
                          }
                        ]
                        ++ mediaMounts
                        ++ (lib.optionals cfg.enableGPU (
                          if cfg.vaapiRenderDevice != "" then
                            [
                              {
                                mountPath = "/dev/dri/renderD128";
                                name = "dri";
                              }
                            ]
                          else
                            [
                              {
                                mountPath = "/dev/dri";
                                name = "dri";
                              }
                            ]
                        ));

                        securityContext = lib.optionalAttrs cfg.enableGPU {
                          capabilities.add = [ "SYS_ADMIN" ];
                          privileged = true;
                        };
                      }
                    ];

                    serviceAccountName = "default";

                    volumes = [
                      cfg.volumes.config.volume
                      cfg.volumes.temp.volume
                    ]
                    ++ mediaVolumes
                    ++ (lib.optionals cfg.enableGPU (
                      if cfg.vaapiRenderDevice != "" then
                        [
                          {
                            name = "dri";
                            hostPath = {
                              path = "/dev/dri/${cfg.vaapiRenderDevice}";
                              type = "CharDevice";
                            };
                          }
                        ]
                      else
                        [
                          {
                            name = "dri";
                            hostPath = {
                              path = "/dev/dri";
                              type = "Directory";
                            };
                          }
                        ]
                    ));
                  };
                };
              };
            };
          };

          ingresses.${name} = with cfg.ingress; {
            metadata.annotations = annotations // {
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
                      pathType = "Prefix";
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

          persistentVolumeClaims =
            lib.optionalAttrs cfg.nfs.enable {
              "${name}-${name}-media-movies".spec = {
                accessModes = [ "ReadWriteMany" ];
                resources.requests.storage = "1Gi";
                storageClassName = "";
                volumeName = "${name}-${name}-media-movies-nfs";
              };
              "${name}-${name}-media-tv".spec = {
                accessModes = [ "ReadWriteMany" ];
                resources.requests.storage = "1Gi";
                storageClassName = "";
                volumeName = "${name}-${name}-media-tv-nfs";
              };
            }
            // lib.optionalAttrs (cfg.nfs.enable && cfg.nfs.enableVideos) {
              "${name}-${name}-media-videos".spec = {
                accessModes = [ "ReadWriteMany" ];
                resources.requests.storage = "1Gi";
                storageClassName = "";
                volumeName = "${name}-${name}-media-videos-nfs";
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
            ];
            selector = {
              "app.kubernetes.io/instance" = name;
              "app.kubernetes.io/name" = name;
            };
            type = "ClusterIP";
          };

          persistentVolumes = lib.optionalAttrs cfg.nfs.enable (
            {
              "${name}-${name}-media-movies-nfs" = {
                apiVersion = "v1";
                kind = "PersistentVolume";
                metadata.name = "${name}-${name}-media-movies-nfs";
                spec = nfsPVOptions // {
                  nfs = {
                    server = cfg.nfs.server;
                    path = "${cfg.nfs.path}/Movies";
                  };
                };
              };
              "${name}-${name}-media-tv-nfs" = {
                apiVersion = "v1";
                kind = "PersistentVolume";
                metadata.name = "${name}-${name}-media-tv-nfs";
                spec = nfsPVOptions // {
                  nfs = {
                    server = cfg.nfs.server;
                    path = "${cfg.nfs.path}/TV";
                  };
                };
              };
            }
            // lib.optionalAttrs cfg.nfs.enableVideos {
              "${name}-${name}-media-videos-nfs" = {
                apiVersion = "v1";
                kind = "PersistentVolume";
                metadata.name = "${name}-${name}-media-videos-nfs";
                spec = nfsPVOptions // {
                  nfs = {
                    server = cfg.nfs.server;
                    path = "${cfg.nfs.path}/Videos";
                  };
                };
              };
            }
          );
        };
    };
}
