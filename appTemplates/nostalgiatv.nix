{ lib, ... }:
with lib;
# NostalgiaTV Server: a live TV guide and HDHR/IPTV tuner built from
# Plex/Jellyfin/Emby libraries, plus the backend for the NostalgiaTV Android
# app. It reads media through those servers' APIs, so it needs no media mount.
# https://hub.docker.com/r/purestream711/nostalgiatv-server
rec {
  name = "nostalgiatv";
  uses-ingress = true;
  uses-replicas = true;

  # One volume carved into the image's three persistent dirs
  # (SQLite DB/index/backups, profiles+channels, channel logos).
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
      default = "purestream711/nostalgiatv-server:0.10.63-20261116-0440";
    };

    service.port = mkOption {
      description = mdDoc "The service port";
      type = types.int;
      default = 19850;
    };

    # The entrypoint starts as root, chowns its dirs to PUID:PGID and drops to them.
    puid = mkOption {
      description = mdDoc "The user ID the server runs as";
      type = types.int;
      default = 1000;
    };

    pgid = mkOption {
      description = mdDoc "The group ID the server runs as";
      type = types.int;
      default = 1000;
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
                    name = "PUID";
                    value = toString cfg.puid;
                  }
                  {
                    name = "PGID";
                    value = toString cfg.pgid;
                  }
                ];
                ports = [
                  {
                    containerPort = cfg.service.port;
                    name = "http";
                    protocol = "TCP";
                  }
                ];
                # The image's own HEALTHCHECK allows 2 minutes to build the content index.
                startupProbe = {
                  httpGet = {
                    path = "/health";
                    port = "http";
                  };
                  periodSeconds = 10;
                  failureThreshold = 30;
                };
                readinessProbe = {
                  httpGet = {
                    path = "/health";
                    port = "http";
                  };
                  periodSeconds = 10;
                };
                livenessProbe = {
                  httpGet = {
                    path = "/health";
                    port = "http";
                  };
                  periodSeconds = 30;
                  failureThreshold = 3;
                };
                resources = {
                  requests = {
                    cpu = "10m";
                    memory = "256Mi";
                  };
                  # ffmpeg is spawned per tuner stream on top of the index.
                  limits.memory = "2Gi";
                };
                volumeMounts = [
                  {
                    name = "data";
                    mountPath = "/app/data";
                    subPath = "data";
                  }
                  {
                    name = "data";
                    mountPath = "/app/config";
                    subPath = "config";
                  }
                  {
                    name = "data";
                    mountPath = "/app/logos";
                    subPath = "logos";
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
