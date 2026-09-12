{ ... }:
{
  flake.nixidyApps.tdarr =
    {
      config,
      lib,
      self,
      ...
    }:
    with lib;
    self.lib.mkArgoApp { inherit config lib self; } rec {
      name = "tdarr";
      uses-ingress = true;

      # Shape only -- no volumeHandle here, that's environment-specific (see
      # env/dev/tdarr.nix and docs/pinned-volumes.md).
      volumes = cfg: {
        config.size = "10Gi";
        temp.size = "50Gi";
      };

      extraOptions = {
        image = mkOption {
          description = mdDoc "The docker image";
          type = types.str;
          default = "ghcr.io/haveagitgat/tdarr:latest";
        };

        service.port = mkOption {
          description = mdDoc "The web UI service port";
          type = types.int;
          default = 8265;
        };

        server.port = mkOption {
          description = mdDoc "The Tdarr server port (for node communication)";
          type = types.int;
          default = 8266;
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
            description = mdDoc "Enable NFS for media volumes";
            type = types.bool;
            default = false;
          };

          server = mkOption {
            description = mdDoc "NFS server hostname/IP";
            type = types.str;
            default = "nasnix";
          };

          path = mkOption {
            description = mdDoc "NFS server base path";
            type = types.str;
            default = "/mnt/media";
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

        internalNode = mkOption {
          description = mdDoc "Enable internal Tdarr node in the server container";
          type = types.bool;
          default = true;
        };

        healthcheckcpuWorkers = mkOption {
          description = mdDoc "Number of CPU workers for media health checks (Quick: HandBrake header scan). Set to 0 to disable.";
          type = types.int;
          default = 1;
        };

        healthcheckgpuWorkers = mkOption {
          description = mdDoc "Number of GPU workers for media health checks (Thorough: FFmpeg frame-by-frame). Set to 0 to disable.";
          type = types.int;
          default = 0;
        };

        transcodecpuWorkers = mkOption {
          description = mdDoc "Number of CPU workers for transcoding. Can be overridden in Tdarr UI.";
          type = types.int;
          default = 1;
        };

        transcodegpuWorkers = mkOption {
          description = mdDoc "Number of GPU workers for transcoding (VAAPI/NVENC). Set to 0 to disable GPU transcodes. Can be overridden in Tdarr UI.";
          type = types.int;
          default = 0;
        };

        ingress.annotations = mkOption {
          description = mdDoc "Annotations for the Ingress resource";
          type = types.attrsOf types.str;
          default = { };
        };

        enableGPU = mkOption {
          description = mdDoc "Enable GPU for hardware transcoding (mounts /dev/dri for AMD/Intel iGPU)";
          type = types.bool;
          default = false;
        };

        enableNvidiaGPU = mkOption {
          description = mdDoc "Request an NVIDIA GPU via nvidia.com/gpu (requires NVIDIA device plugin). Set false for AMD/Intel only.";
          type = types.bool;
          default = false;
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
                  automountServiceAccountToken = true;
                  securityContext = {
                    fsGroup = cfg.pgid;
                    fsGroupChangePolicy = "OnRootMismatch";
                    # With GPU: run as root so main process and all subworkers can access /dev/dri (image may drop to PUID and strip supplementalGroups).
                    # Without GPU: run as tdarr user.
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
                          name = "serverIP";
                          value = "0.0.0.0";
                        }
                        {
                          name = "serverPort";
                          value = toString cfg.server.port;
                        }
                        {
                          name = "webUIPort";
                          value = toString cfg.service.port;
                        }
                        {
                          name = "internalNode";
                          value = if cfg.internalNode then "true" else "false";
                        }
                        {
                          name = "inContainer";
                          value = "true";
                        }
                        {
                          name = "auth";
                          value = "false";
                        }
                        {
                          name = "healthcheckcpuWorkers";
                          value = toString cfg.healthcheckcpuWorkers;
                        }
                        {
                          name = "healthcheckgpuWorkers";
                          value = toString cfg.healthcheckgpuWorkers;
                        }
                        {
                          name = "transcodecpuWorkers";
                          value = toString cfg.transcodecpuWorkers;
                        }
                        {
                          name = "transcodegpuWorkers";
                          value = toString cfg.transcodegpuWorkers;
                        }
                      ]
                      ++ (lib.optionals (cfg.libvaDriverName != "") [
                        {
                          name = "LIBVA_DRIVER_NAME";
                          value = cfg.libvaDriverName;
                        }
                      ])
                      ++ (lib.optionals cfg.enableGPU [
                        # Avoid "resource allocation failed" (vaInitialize error 2) in headless/container.
                        {
                          name = "XDG_RUNTIME_DIR";
                          value = "";
                        }
                        # Help libva find container's VA drivers (Debian/Ubuntu path; image may use this).
                        {
                          name = "LIBVA_DRIVERS_PATH";
                          value = "/usr/lib/x86_64-linux-gnu/dri";
                        }
                        # Verbose libva messages to diagnose "No VA display found" (set to "0" to quiet).
                        {
                          name = "LIBVA_MESSAGING";
                          value = "1";
                        }
                      ])
                      ++ (lib.optionals cfg.enableNvidiaGPU [
                        {
                          name = "NVIDIA_VISIBLE_DEVICES";
                          value = "all";
                        }
                        {
                          name = "NVIDIA_DRIVER_CAPABILITIES";
                          value = "all";
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
                          value = "localhost,127.0.0.1,.svc,.svc.cluster.local";
                        }
                      ]);
                      ports = [
                        {
                          containerPort = cfg.service.port;
                          name = "http";
                          protocol = "TCP";
                        }
                        {
                          containerPort = cfg.server.port;
                          name = "server";
                          protocol = "TCP";
                        }
                      ];
                      readinessProbe = lib.mkIf cfg.useProbes {
                        httpGet = {
                          path = "/";
                          port = cfg.service.port;
                        };
                        initialDelaySeconds = 180;
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
                        initialDelaySeconds = 300;
                        periodSeconds = 30;
                        timeoutSeconds = 10;
                        successThreshold = 1;
                        failureThreshold = 3;
                      };
                      volumeMounts = [
                        {
                          mountPath = "/app/server";
                          name = "config";
                        }
                        {
                          mountPath = "/temp";
                          name = "temp";
                        }
                        {
                          mountPath = "/media/Movies";
                          name = "media-movies";
                        }
                        {
                          mountPath = "/media/TV";
                          name = "media-tv";
                        }
                      ]
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
                      resources = lib.optionalAttrs cfg.enableNvidiaGPU {
                        limits."nvidia.com/gpu" = "1";
                      };
                      securityContext = lib.optionalAttrs cfg.enableGPU {
                        capabilities.add = [ "SYS_ADMIN" ];
                        # Required for some runtimes to open hostPath DRI device ("Failed to open the given device").
                        privileged = true;
                      };
                    }
                  ];

                  serviceAccountName = "default";
                  initContainers = [
                    {
                      name = "init-temp-cache";
                      image = "busybox:latest";
                      imagePullPolicy = "IfNotPresent";
                      command = [
                        "sh"
                        "-c"
                        "mkdir -p /temp/cache && chown -R ${toString cfg.puid}:${toString cfg.pgid} /temp/cache"
                      ];
                      securityContext.runAsUser = 0;
                      volumeMounts = [
                        {
                          mountPath = "/temp";
                          name = "temp";
                        }
                      ];
                    }
                  ]
                  ++ (lib.optionals cfg.vpn.enable (
                    self.lib.waitForGluetun { inherit lib; } cfg.vpn.sharedGluetunService
                  ));

                  volumes = [
                    cfg.volumes.config.volume
                    cfg.volumes.temp.volume
                    {
                      name = "media-movies";
                      persistentVolumeClaim.claimName = "${name}-${name}-media-movies";
                    }
                    {
                      name = "media-tv";
                      persistentVolumeClaim.claimName = "${name}-${name}-media-tv";
                    }
                  ]
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
          metadata.annotations =
            lib.optionalAttrs (clusterIssuer != "") {
              "cert-manager.io/cluster-issuer" = clusterIssuer;
            }
            // annotations;
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
                secretName = "${domain}-tls";
              }
            ];
          };
        };

        persistentVolumeClaims = {
          "${name}-${name}-media-movies".spec =
            if cfg.nfs.enable then
              {
                accessModes = [ "ReadWriteMany" ];
                resources.requests.storage = "1Gi";
                storageClassName = "";
                volumeName = "${name}-${name}-media-movies-nfs";
              }
            else
              {
                inherit (cfg) storageClassName;
                accessModes = [ "ReadWriteOnce" ];
                resources.requests.storage = "100Gi";
              };
          "${name}-${name}-media-tv".spec =
            if cfg.nfs.enable then
              {
                accessModes = [ "ReadWriteMany" ];
                resources.requests.storage = "1Gi";
                storageClassName = "";
                volumeName = "${name}-${name}-media-tv-nfs";
              }
            else
              {
                inherit (cfg) storageClassName;
                accessModes = [ "ReadWriteOnce" ];
                resources.requests.storage = "100Gi";
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
              name = "server";
              port = cfg.server.port;
              protocol = "TCP";
              targetPort = "server";
            }
          ];
          selector = {
            "app.kubernetes.io/instance" = name;
            "app.kubernetes.io/name" = name;
          };
          type = "ClusterIP";
        };

        # Create NFS PersistentVolumes for media when NFS is enabled
        persistentVolumes = lib.optionalAttrs (cfg.nfs.enable) {
          "${name}-${name}-media-movies-nfs" = {
            apiVersion = "v1";
            kind = "PersistentVolume";
            metadata = {
              name = "${name}-${name}-media-movies-nfs";
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
                path = "${cfg.nfs.path}/Movies";
              };
              persistentVolumeReclaimPolicy = "Retain";
            };
          };
          "${name}-${name}-media-tv-nfs" = {
            apiVersion = "v1";
            kind = "PersistentVolume";
            metadata = {
              name = "${name}-${name}-media-tv-nfs";
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
                path = "${cfg.nfs.path}/TV";
              };
              persistentVolumeReclaimPolicy = "Retain";
            };
          };
        };
      };
    };
}
