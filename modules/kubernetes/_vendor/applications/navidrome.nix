{ ... }:
{
  flake.nixidyApps.navidrome =
    {
      config,
      lib,
      self,
      ...
    }:
    with lib;
    self.lib.mkArgoApp { inherit config lib self; } rec {
      name = "navidrome";
      uses-ingress = true;
      uses-nfs = true;

      # Shape only -- no volumeHandle here, that's environment-specific (see
      # env/dev/navidrome.nix and docs/pinned-volumes.md).
      volumes = cfg: {
        data.size = "5Gi";
      };

      extraOptions = {
        image = mkOption {
          description = mdDoc "The docker image";
          type = types.str;
          default = "deluan/navidrome:latest";
        };

        service.port = mkOption {
          description = mdDoc "The service port";
          type = types.int;
          default = 4533;
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
      };

      extraResources = cfg: {
        deployments.${name} = {
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
                securityContext = {
                  runAsUser = cfg.puid;
                  runAsGroup = cfg.pgid;
                  fsGroup = cfg.pgid;
                };

                containers = [
                  {
                    inherit name;
                    image = cfg.image;
                    imagePullPolicy = "IfNotPresent";
                    env = [
                      {
                        name = "ND_MUSICFOLDER";
                        value = "/music";
                      }
                      {
                        name = "ND_DATAFOLDER";
                        value = "/data";
                      }
                      {
                        name = "TZ";
                        value = cfg.tz;
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
                        path = "/ping";
                        port = cfg.service.port;
                      };
                      initialDelaySeconds = 15;
                      periodSeconds = 10;
                      timeoutSeconds = 5;
                      successThreshold = 1;
                      failureThreshold = 3;
                    };
                    livenessProbe = {
                      httpGet = {
                        path = "/ping";
                        port = cfg.service.port;
                      };
                      initialDelaySeconds = 30;
                      periodSeconds = 30;
                      timeoutSeconds = 5;
                      successThreshold = 1;
                      failureThreshold = 3;
                    };
                    volumeMounts = [
                      {
                        mountPath = "/data";
                        name = "data";
                      }
                      {
                        mountPath = "/music";
                        name = "music";
                        readOnly = true;
                      }
                    ];
                  }
                ];

                volumes = [
                  cfg.volumes.data.volume
                  {
                    name = "music";
                    persistentVolumeClaim.claimName = "${name}-${name}-music";
                  }
                ];
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

        persistentVolumeClaims = {
          "${name}-${name}-music".spec =
            if cfg.nfs.enable then
              {
                accessModes = [ "ReadOnlyMany" ];
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

        persistentVolumes = lib.optionalAttrs cfg.nfs.enable {
          "${name}-${name}-music-nfs" = {
            apiVersion = "v1";
            kind = "PersistentVolume";
            metadata = {
              name = "${name}-${name}-music-nfs";
            };
            spec = {
              capacity.storage = "1Ti";
              accessModes = [ "ReadOnlyMany" ];
              mountOptions = [
                "nolock"
                "noexec"
                "soft"
                "timeo=30"
              ];
              nfs = {
                server = cfg.nfs.server;
                path = cfg.nfs.path;
                readOnly = true;
              };
              persistentVolumeReclaimPolicy = "Retain";
            };
          };
        };
      };
    };
}
