{ ... }:
{
  flake.nixidyApps.audiobookshelf =
    {
      config,
      lib,
      self,
      ...
    }:
    with lib;
    let
      cfg = config.services.audiobookshelf;
    in
    self.lib.mkArgoApp { inherit config lib self; } rec {
      name = "audiobookshelf";
      uses-ingress = true;
      uses-nfs = true;

      # Shape only -- no volumeHandle here, that's environment-specific (see
      # env/dev/audiobookshelf.nix and docs/pinned-volumes.md).
      volumes = cfg: {
        config.size = "1Gi";
        metadata.size = "5Gi";
      };

      extraOptions = {
        image = mkOption {
          description = mdDoc "The docker image";
          type = types.str;
          default = "ghcr.io/advplyr/audiobookshelf:latest";
        };

        apiKey = mkOption {
          description = mdDoc ''
            Audiobookshelf API token (web UI -> user account settings). Stored in
            secrets.enc.yaml as `audiobookshelf.key` and wired in via
            `env/dev/audiobookshelf.nix`. Only powers the auto-added homepage
            dashboard widget below -- never injected into the audiobookshelf
            container itself.
          '';
          type = types.str;
          default = "";
        };

        # Auto-add an Audiobookshelf widget to this app's homepage dashboard
        # tile once an API key is configured -- see applications/immich.nix for
        # the same pattern with more detail. Set
        # `services.homepage.widgetSecrets.AUDIOBOOKSHELF_API_KEY` from
        # `config.services.audiobookshelf.apiKey` in env/dev/homepage.nix.
        homepage.extraSettings = mkOption {
          default = lib.optionalAttrs (cfg.apiKey != "") {
            widget = {
              type = "audiobookshelf";
              url = "http://${name}.${cfg.namespace}:${toString cfg.service.port}";
              key = "{{HOMEPAGE_VAR_AUDIOBOOKSHELF_API_KEY}}";
            };
          };
        };

        service.port = mkOption {
          description = mdDoc "The service port";
          type = types.int;
          default = 80;
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

                containers = [
                  {
                    inherit name;
                    image = cfg.image;
                    imagePullPolicy = "IfNotPresent";
                    env = [
                      {
                        name = "PGID";
                        value = toString cfg.pgid;
                      }
                      {
                        name = "PUID";
                        value = toString cfg.puid;
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
                        path = "/healthcheck";
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
                        path = "/healthcheck";
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
                        mountPath = "/config";
                        name = "config";
                      }
                      {
                        mountPath = "/metadata";
                        name = "metadata";
                      }
                      {
                        mountPath = "/audiobooks";
                        name = "audiobooks";
                      }
                    ];
                  }
                ];

                volumes = [
                  cfg.volumes.config.volume
                  cfg.volumes.metadata.volume
                  {
                    name = "audiobooks";
                    persistentVolumeClaim.claimName = "${name}-${name}-audiobooks";
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

        persistentVolumeClaims."${name}-${name}-audiobooks".spec =
          if cfg.nfs.enable then
            {
              accessModes = [ "ReadWriteMany" ];
              resources.requests.storage = "1Gi";
              storageClassName = "";
              volumeName = "${name}-${name}-audiobooks-nfs";
            }
          else
            {
              inherit (cfg) storageClassName;
              accessModes = [ "ReadWriteOnce" ];
              resources.requests.storage = "100Gi";
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
          "${name}-${name}-audiobooks-nfs" = {
            apiVersion = "v1";
            kind = "PersistentVolume";
            metadata = {
              name = "${name}-${name}-audiobooks-nfs";
            };
            spec = {
              capacity.storage = "1Ti";
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
