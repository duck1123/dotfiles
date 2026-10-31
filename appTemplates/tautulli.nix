{ config, lib, ... }:
with lib;
let
  cfg = config.services.tautulli;
in
rec {
  name = "tautulli";
  uses-ingress = true;
  uses-replicas = true;

  # SQLite db, logs and backups -- see docs/pinned-volumes.md to pin it.
  volumes = cfg: {
    config.size = "2Gi";
  };

  extraOptions = {
    image = mkOption {
      description = mdDoc "The docker image";
      type = types.str;
      default = "linuxserver/tautulli:latest";
    };

    service.port = mkOption {
      description = mdDoc "The service port";
      type = types.int;
      default = 8181;
    };

    apiKey = mkOption {
      description = mdDoc ''
        Tautulli API key (Settings -> Web Interface -> API). Only powers the
        auto-added homepage dashboard widget below -- never injected into
        the tautulli container itself.
      '';
      type = types.str;
      default = "";
    };

    # Auto-add a Tautulli widget to this app's homepage dashboard tile once
    # an API key is configured -- see appTemplates/sonarr.nix. Set
    # `services.homepage.widgetSecrets.TAUTULLI_API_KEY` from
    # `config.services.tautulli.apiKey` in clusters/dev/apps/homepage.nix.
    homepage.extraSettings = mkOption {
      default = lib.optionalAttrs (cfg.apiKey != "") {
        widget = {
          type = "tautulli";
          url = "http://${name}.${cfg.namespace}:${toString cfg.service.port}";
          key = "{{HOMEPAGE_VAR_TAUTULLI_API_KEY}}";
        };
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
  };

  extraResources = cfg: {
    deployments.${name} = {
      metadata.labels = {
        "app.kubernetes.io/instance" = name;
        "app.kubernetes.io/name" = name;
      };

      spec = {
        inherit (cfg) replicas;
        # SQLite on a RWO volume: never run two pods at once.
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
                    path = "/status";
                    port = cfg.service.port;
                  };
                  initialDelaySeconds = 15;
                  periodSeconds = 10;
                  timeoutSeconds = 5;
                  failureThreshold = 6;
                };
                livenessProbe = {
                  httpGet = {
                    path = "/status";
                    port = cfg.service.port;
                  };
                  initialDelaySeconds = 60;
                  periodSeconds = 30;
                  timeoutSeconds = 5;
                  failureThreshold = 3;
                };
                volumeMounts = [
                  {
                    mountPath = "/config";
                    name = "config";
                  }
                ];
              }
            ];

            volumes = [ cfg.volumes.config.volume ];
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
}
