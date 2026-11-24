{ lib, ... }:
with lib;
# Libre Closet: a self-hosted wardrobe organizer and outfit planner.
# SQLite and uploaded photos both live under DATA_PATH, so one volume holds everything.
# https://github.com/Lazztech/Libre-Closet
rec {
  name = "libre-closet";
  uses-ingress = true;
  uses-replicas = true;

  volumes = cfg: {
    data = {
      size = "5Gi";
      volumeAttributes.backupTargetName = "default";
    };
  };

  extraOptions = {
    image = mkOption {
      description = mdDoc "The docker image";
      type = types.str;
      default = "ghcr.io/lazztech/libre-closet:0.6.0";
    };

    service.port = mkOption {
      description = mdDoc "The service port";
      type = types.int;
      default = 3000;
    };
  };

  extraResources = cfg: {
    deployments.${name} = {
      metadata.labels = {
        "app.kubernetes.io/instance" = name;
        "app.kubernetes.io/name" = name;
      };

      spec = {
        inherit (cfg) replicas;
        # The data volume is ReadWriteOnce and holds a SQLite DB.
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
            containers = [
              {
                inherit name;
                inherit (cfg) image;
                imagePullPolicy = "IfNotPresent";
                env = [
                  {
                    name = "TZ";
                    value = cfg.tz;
                  }
                  {
                    name = "DATA_PATH";
                    value = "/app/data";
                  }
                  {
                    name = "PORT";
                    value = toString cfg.service.port;
                  }
                  # Single-user mode, as in upstream's compose example. Turning
                  # auth on also needs ACCESS_TOKEN_SECRET set from a secret.
                  {
                    name = "AUTH_ENABLED";
                    value = "false";
                  }
                  {
                    name = "PWA_ENABLED";
                    value = "true";
                  }
                ];
                ports = [
                  {
                    containerPort = cfg.service.port;
                    name = "http";
                    protocol = "TCP";
                  }
                ];
                startupProbe = {
                  tcpSocket.port = "http";
                  periodSeconds = 5;
                  failureThreshold = 24;
                };
                readinessProbe = {
                  httpGet = {
                    path = "/";
                    port = "http";
                  };
                  periodSeconds = 10;
                };
                livenessProbe = {
                  tcpSocket.port = "http";
                  periodSeconds = 30;
                  failureThreshold = 3;
                };
                resources = {
                  requests = {
                    cpu = "10m";
                    memory = "128Mi";
                  };
                  limits.memory = "1Gi";
                };
                volumeMounts = [
                  {
                    name = "data";
                    mountPath = "/app/data";
                  }
                ];
              }
            ];

            volumes = [ cfg.volumes.data.volume ];
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
            inherit (tls) secretName;
          }
        ];
      };
    };
  };
}
