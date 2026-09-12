{ ... }:
{
  flake.nixidyApps.trilium =
    {
      config,
      lib,
      self,
      ...
    }:
    with lib;
    let
      cfg = config.services.trilium;
    in
    self.lib.mkArgoApp { inherit config lib self; } rec {
      name = "trilium";
      uses-ingress = true;

      # Shape only -- no volumeHandle here, that's environment-specific (see
      # env/dev/trilium.nix and docs/pinned-volumes.md).
      volumes = cfg: {
        data.size = cfg.dataStorage;
      };

      extraOptions = {
        image = mkOption {
          description = mdDoc "The docker image";
          type = types.str;
          default = "triliumnext/trilium:v0.105.0";
        };

        service.port = mkOption {
          description = mdDoc "The service port";
          type = types.int;
          default = 8080;
        };

        apiKey = mkOption {
          description = mdDoc ''
            Trilium ETAPI token (Options -> ETAPI -> Create new ETAPI token).
            Stored in secrets.enc.yaml as `trilium.key` and wired in via
            `env/dev/trilium.nix`. Only powers the auto-added homepage
            dashboard widget below -- never injected into the trilium
            container itself.
          '';
          type = types.str;
          default = "";
        };

        # Auto-add a Trilium widget to this app's homepage dashboard tile once
        # an ETAPI token is configured -- see applications/immich.nix for the
        # same pattern with more detail. Set
        # `services.homepage.widgetSecrets.TRILIUM_API_KEY` from
        # `config.services.trilium.apiKey` in env/dev/homepage.nix.
        homepage.extraSettings = mkOption {
          default = lib.optionalAttrs (cfg.apiKey != "") {
            widget = {
              type = "trilium";
              url = "http://${name}.${cfg.namespace}:${toString cfg.service.port}";
              key = "{{HOMEPAGE_VAR_TRILIUM_API_KEY}}";
            };
          };
        };

        dataStorage = mkOption {
          description = mdDoc "Size of the data PVC";
          type = types.str;
          default = "10Gi";
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
                # Kubernetes injects <SERVICE_NAME>_PORT env vars into every pod; since the
                # Service is named "trilium" this collides with Trilium's own TRILIUM_PORT
                # env var (which expects a plain int, not the injected tcp:// URI).
                enableServiceLinks = false;

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
                    ];
                    ports = [
                      {
                        containerPort = cfg.service.port;
                        name = "http";
                        protocol = "TCP";
                      }
                    ];
                    readinessProbe = {
                      tcpSocket.port = cfg.service.port;
                      initialDelaySeconds = 15;
                      periodSeconds = 10;
                      timeoutSeconds = 5;
                      successThreshold = 1;
                      failureThreshold = 3;
                    };
                    livenessProbe = {
                      tcpSocket.port = cfg.service.port;
                      initialDelaySeconds = 30;
                      periodSeconds = 30;
                      timeoutSeconds = 5;
                      successThreshold = 1;
                      failureThreshold = 3;
                    };
                    volumeMounts = [
                      {
                        mountPath = "/home/node/trilium-data";
                        name = "data";
                      }
                    ];
                  }
                ];

                volumes = [ cfg.volumes.data.volume ];
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
