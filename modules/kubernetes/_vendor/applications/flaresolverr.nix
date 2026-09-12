{ ... }:
{
  flake.nixidyApps.flaresolverr =
    {
      config,
      lib,
      self,
      ...
    }:
    with lib;
    self.lib.mkArgoApp { inherit config lib; } rec {
      name = "flaresolverr";
      uses-ingress = false;

      extraOptions = {
        image = mkOption {
          description = mdDoc "The docker image";
          type = types.str;
          default = "ghcr.io/flaresolverr/flaresolverr:v3.5.0";
        };

        service.port = mkOption {
          description = mdDoc "The service port";
          type = types.int;
          default = 8191;
        };

        logLevel = mkOption {
          description = mdDoc "Log level (error, warning, info, debug, trace)";
          type = types.str;
          default = "info";
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
                          name = "LOG_LEVEL";
                          value = cfg.logLevel;
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
                          path = "/health";
                          port = cfg.service.port;
                        };
                        initialDelaySeconds = 20;
                        periodSeconds = 10;
                        timeoutSeconds = 5;
                        successThreshold = 1;
                        failureThreshold = 3;
                      };
                      livenessProbe = {
                        httpGet = {
                          path = "/health";
                          port = cfg.service.port;
                        };
                        initialDelaySeconds = 30;
                        periodSeconds = 30;
                        timeoutSeconds = 5;
                        successThreshold = 1;
                        failureThreshold = 3;
                      };
                      resources = {
                        requests = {
                          memory = "256Mi";
                          cpu = "100m";
                        };
                        limits = {
                          memory = "1Gi";
                          cpu = "1000m";
                        };
                      };
                    }
                  ];
                };
              };
            };
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
