{ ... }:
{
  flake.nixidyApps.prowlarr =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      password-secret = "prowlarr-database-password";
      cfg = config.services.prowlarr;
    in
    self.lib.mkArgoApp
      {
        inherit
          config
          lib
          self
          pkgs
          ;
      }
      rec {
        name = "prowlarr";
        uses-ingress = true;
        uses-database = true;

        # Shape only -- no volumeHandle here, that's environment-specific (see
        # env/dev/prowlarr.nix and docs/pinned-volumes.md).
        volumes = cfg: {
          config.size = "5Gi";
        };

        extraOptions = {
          image = mkOption {
            description = mdDoc "The docker image";
            type = types.str;
            default = "linuxserver/prowlarr:latest";
          };

          service.port = mkOption {
            description = mdDoc "The service port";
            type = types.int;
            default = 9696;
          };

          apiKey = mkOption {
            description = mdDoc ''
              Prowlarr API key (Settings -> General -> Security). Stored in
              secrets.enc.yaml as `prowlarr.key` and wired in via
              `env/dev/prowlarr.nix`. Only powers the auto-added homepage
              dashboard widget below -- never injected into the prowlarr
              container itself.
            '';
            type = types.str;
            default = "";
          };

          # Auto-add a Prowlarr widget to this app's homepage dashboard tile
          # once an API key is configured -- see applications/immich.nix for the
          # same pattern with more detail. Set
          # `services.homepage.widgetSecrets.PROWLARR_API_KEY` from
          # `config.services.prowlarr.apiKey` in env/dev/homepage.nix.
          homepage.extraSettings = mkOption {
            default = lib.optionalAttrs (cfg.apiKey != "") {
              widget = {
                type = "prowlarr";
                url = "http://${name}.${cfg.namespace}:${toString cfg.service.port}";
                key = "{{HOMEPAGE_VAR_PROWLARR_API_KEY}}";
              };
            };
          };

          useProbes = mkOption {
            description = mdDoc "Enable readiness and liveness probes";
            type = types.bool;
            default = true;
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

        sopsSecrets =
          cfg:
          lib.optionalAttrs (cfg.database.enable && cfg.database.password != "") {
            ${password-secret} = {
              password = cfg.database.password;
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
                    initContainers = lib.optionalAttrs cfg.vpn.enable (
                      self.lib.waitForGluetun { inherit lib; } cfg.vpn.sharedGluetunService
                    );
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
                        ]
                        ++ (lib.optionals cfg.database.enable [
                          {
                            name = "PROWLARR__POSTGRES__HOST";
                            value = cfg.database.host;
                          }
                          {
                            name = "PROWLARR__POSTGRES__PORT";
                            value = toString cfg.database.port;
                          }
                          {
                            name = "PROWLARR__POSTGRES__MAINDB";
                            value = cfg.database.name;
                          }
                          {
                            name = "PROWLARR__POSTGRES__LOGDB";
                            value =
                              if lib.hasSuffix "-main" cfg.database.name then
                                lib.removeSuffix "-main" cfg.database.name + "-log"
                              else
                                "${cfg.database.name}-log";
                          }
                          {
                            name = "PROWLARR__POSTGRES__USER";
                            value = cfg.database.username;
                          }
                          (
                            if cfg.database.password != "" then
                              {
                                name = "PROWLARR__POSTGRES__PASSWORD";
                                valueFrom = {
                                  secretKeyRef = {
                                    name = password-secret;
                                    key = "password";
                                  };
                                };
                              }
                            else
                              {
                                name = "PROWLARR__POSTGRES__PASSWORD";
                                value = "";
                              }
                          )
                        ])
                        ++ (lib.optionals cfg.vpn.enable [
                          # Configure Prowlarr to use shared gluetun's HTTP proxy
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
                            value = "localhost,127.0.0.1,.svc,.svc.cluster.local,sabnzbd.sabnzbd,sabnzbd.sabnzbd.svc.cluster.local,qbittorrent.qbittorrent,qbittorrent.qbittorrent.svc.cluster.local";
                          }
                        ]);
                        ports = [
                          {
                            containerPort = cfg.service.port;
                            name = "http";
                            protocol = "TCP";
                          }
                        ];
                        readinessProbe = lib.mkIf cfg.useProbes {
                          httpGet = {
                            path = "/";
                            port = cfg.service.port;
                          };
                          initialDelaySeconds = 60;
                          periodSeconds = 10;
                          timeoutSeconds = 5;
                          successThreshold = 1;
                          failureThreshold = 3;
                        };
                        livenessProbe = lib.mkIf cfg.useProbes {
                          httpGet = {
                            path = "/";
                            port = cfg.service.port;
                          };
                          initialDelaySeconds = 90;
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
                        ];
                      }
                    ];
                    volumes = [
                      cfg.volumes.config.volume
                    ]
                    ++ (lib.optionals (cfg.database.enable && cfg.database.password != "") [
                      {
                        name = password-secret;
                        secret.secretName = password-secret;
                      }
                    ]);
                  };
                };
              };
            };
          };

          ingresses.${name} = with cfg.ingress; {
            metadata.annotations = lib.optionalAttrs (clusterIssuer != "") {
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
      };
}
