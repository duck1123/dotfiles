{ ... }:
{
  # https://wiki.kavitareader.com/
  flake.nixidyApps.kavita =
    {
      config,
      lib,
      self,
      ...
    }:
    with lib;
    self.lib.mkArgoApp { inherit config lib; } rec {
      name = "kavita";
      uses-ingress = true;

      extraResources = cfg: {
        deployments.kavita = {
          metadata = {
            labels = {
              "app.kubernetes.io/instance" = name;
              "app.kubernetes.io/name" = name;
              "app.kubernetes.io/version" = "v0.9.1.4";
            };
          };
          spec = {
            strategy.type = "Recreate";
            selector.matchLabels = {
              "app.kubernetes.io/instance" = name;
              "app.kubernetes.io/name" = name;
            };

            template = {
              metadata = {
                labels = {
                  "app.kubernetes.io/instance" = name;
                  "app.kubernetes.io/name" = name;
                  # "app.kubernetes.io/version" = "v0.9.1.4";
                };
              };

              spec = {
                automountServiceAccountToken = true;
                serviceAccountName = "default";
                containers = [
                  {
                    inherit name;
                    image = "linuxserver/kavita:v0.9.1.4-ls123";
                    imagePullPolicy = "IfNotPresent";
                    env = [
                      {
                        name = "PGID";
                        value = "1000";
                      }
                      {
                        name = "PUID";
                        value = "1000";
                      }
                      {
                        name = "TZ";
                        value = "Etc/UTC";
                      }
                    ];

                    livenessProbe = {
                      failureThreshold = 3;
                      initialDelaySeconds = 0;
                      periodSeconds = 10;
                      tcpSocket.port = 5000;
                    };

                    ports = [
                      {
                        containerPort = 5000;
                        name = "http";
                        protocol = "TCP";
                      }
                    ];

                    volumeMounts = [
                      {
                        mountPath = "/books";
                        name = "books";
                      }
                      {
                        mountPath = "/config";
                        name = "config";
                      }
                    ];
                  }
                ];
                volumes = [
                  {
                    name = "books";
                    persistentVolumeClaim.claimName = "${name}-${name}-books";
                  }
                  {
                    name = "config";
                    persistentVolumeClaim.claimName = "${name}-${name}-config";
                  }
                ];
              };
            };
          };
        };

        ingresses.${name} = with cfg.ingress; {
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
            tls = [ { hosts = [ domain ]; } ];
          };
        };

        persistentVolumeClaims = {
          "${name}-${name}-books".spec = {
            accessModes = [ "ReadWriteOnce" ];
            resources.requests.storage = "5Gi";
            storageClassName = cfg.storageClassName;
          };
          "${name}-${name}-config".spec = {
            accessModes = [ "ReadWriteOnce" ];
            resources.requests.storage = "5Gi";
            storageClassName = cfg.storageClassName;
          };
        };

        services.${name}.spec = {
          ports = [
            {
              name = "http";
              port = 5000;
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
