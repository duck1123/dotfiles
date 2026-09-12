{ ... }:
{
  flake.nixidyApps.stashapp =
    {
      config,
      lib,
      self,
      ...
    }:
    with lib;
    let
      cfg = config.services.stashapp;
    in
    self.lib.mkArgoApp { inherit config lib self; } rec {
      name = "stashapp";
      uses-ingress = true;

      # Shape only -- no volumeHandle here, that's environment-specific (see
      # env/dev/stashapp.nix and docs/pinned-volumes.md).
      volumes = cfg: {
        config.size = "200Gi";
      };

      extraOptions = {
        image = mkOption {
          description = mdDoc "The docker image";
          type = types.str;
          default = "stashapp/stash:latest";
        };

        service.port = mkOption {
          description = mdDoc "The service port";
          type = types.int;
          default = 9999;
        };

        apiKey = mkOption {
          description = mdDoc ''
            Stash API key (Settings -> Security -> API Key). Stored in
            secrets.enc.yaml as `stashapp.key` and wired in via
            `env/dev/stashapp.nix`. Only powers the auto-added homepage
            dashboard widget below -- never injected into the stashapp
            container itself.
          '';
          type = types.str;
          default = "";
        };

        # Auto-add a Stash widget to this app's homepage dashboard tile once an
        # API key is configured -- see applications/immich.nix for the same
        # pattern with more detail. Note homepage's widget type is "stash", not
        # "stashapp". Set `services.homepage.widgetSecrets.STASHAPP_API_KEY`
        # from `config.services.stashapp.apiKey` in env/dev/homepage.nix.
        homepage.extraSettings = mkOption {
          default = lib.optionalAttrs (cfg.apiKey != "") {
            widget = {
              type = "stash";
              url = "http://${name}.${cfg.namespace}:${toString cfg.service.port}";
              key = "{{HOMEPAGE_VAR_STASHAPP_API_KEY}}";
            };
          };
        };

        nfs = {
          enable = mkOption {
            description = mdDoc "Enable NFS for data volume";
            type = types.bool;
            default = true;
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

        enableGPU = mkOption {
          description = mdDoc "Enable shared GPU for hardware encoding (mounts /dev/dri)";
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
                  serviceAccountName = "default";
                }
                // {
                  containers = [
                    (
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
                            name = "STASH_STASH_DIR";
                            value = "/data";
                          }
                          {
                            name = "FFMPEG_EXE";
                            value = "/usr/bin/ffmpeg";
                          }
                          {
                            name = "FFPROBE_EXE";
                            value = "/usr/bin/ffprobe";
                          }
                        ];
                        ports = [
                          {
                            containerPort = cfg.service.port;
                            name = "http";
                            protocol = "TCP";
                          }
                        ];
                        readinessProbe = {
                          httpGet = {
                            path = "/";
                            port = cfg.service.port;
                          };
                          initialDelaySeconds = 30;
                          periodSeconds = 10;
                          timeoutSeconds = 5;
                          successThreshold = 1;
                          failureThreshold = 3;
                        };
                        livenessProbe = {
                          httpGet = {
                            path = "/";
                            port = cfg.service.port;
                          };
                          initialDelaySeconds = 60;
                          periodSeconds = 30;
                          timeoutSeconds = 5;
                          successThreshold = 1;
                          failureThreshold = 3;
                        };
                        volumeMounts = [
                          {
                            mountPath = "/root/.stash";
                            name = "config";
                          }
                          {
                            mountPath = "/data";
                            name = "data";
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
                      }
                      // (lib.optionalAttrs cfg.enableGPU {
                        securityContext = {
                          privileged = false;
                          capabilities = {
                            add = [ "SYS_ADMIN" ];
                          };
                        };
                      })
                    )
                  ];
                  volumes = [
                    cfg.volumes.config.volume
                    {
                      name = "data";
                      persistentVolumeClaim.claimName = "${name}-${name}-data";
                    }
                  ]
                  ++ (lib.optionals cfg.enableGPU (
                    if cfg.vaapiRenderDevice != "" then
                      [
                        {
                          name = "dri";
                          hostPath.path = "/dev/dri/${cfg.vaapiRenderDevice}";
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
          "${name}-${name}-data".spec =
            if cfg.nfs.enable then
              {
                accessModes = [ "ReadWriteMany" ];
                resources.requests.storage = "1Gi";
                storageClassName = "";
                volumeName = "${name}-${name}-data-nfs";
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
          ];

          selector = {
            "app.kubernetes.io/instance" = name;
            "app.kubernetes.io/name" = name;
          };

          type = "ClusterIP";
        };

        # Create NFS PersistentVolume for data when NFS is enabled
        persistentVolumes = lib.optionalAttrs cfg.nfs.enable {
          "${name}-${name}-data-nfs" = {
            apiVersion = "v1";
            kind = "PersistentVolume";
            metadata = {
              name = "${name}-${name}-data-nfs";
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
                path = cfg.nfs.path;
              };
              persistentVolumeReclaimPolicy = "Retain";
            };
          };
        };
      };
    };
}
