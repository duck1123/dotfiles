{ ... }:
{
  flake.nixidyApps.radarr =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      password-secret = "radarr-database-password";
      cfg = config.services.radarr;
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
        name = "radarr";
        uses-ingress = true;
        uses-database = true;

        # Shape only -- no volumeHandle here, that's environment-specific (see
        # env/dev/radarr.nix and docs/pinned-volumes.md).
        volumes = cfg: {
          config.size = "5Gi";
        };

        extraOptions = {
          image = mkOption {
            description = mdDoc "The docker image";
            type = types.str;
            default = "linuxserver/radarr:latest";
          };

          service.port = mkOption {
            description = mdDoc "The service port";
            type = types.int;
            default = 7878;
          };

          apiKey = mkOption {
            description = mdDoc ''
              Radarr API key (Settings -> General -> Security). Stored in
              secrets.enc.yaml as `radarr.key` and wired in via
              `env/dev/radarr.nix`. Only powers the auto-added homepage
              dashboard widget below -- never injected into the radarr
              container itself.
            '';
            type = types.str;
            default = "";
          };

          # Auto-add a Radarr widget to this app's homepage dashboard tile once
          # an API key is configured -- see applications/immich.nix for the same
          # pattern with more detail. Set
          # `services.homepage.widgetSecrets.RADARR_API_KEY` from
          # `config.services.radarr.apiKey` in env/dev/homepage.nix.
          homepage.extraSettings = mkOption {
            default = lib.optionalAttrs (cfg.apiKey != "") {
              widget = {
                type = "radarr";
                url = "http://${name}.${cfg.namespace}:${toString cfg.service.port}";
                key = "{{HOMEPAGE_VAR_RADARR_API_KEY}}";
              };
            };
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

          nfs = {
            enable = mkOption {
              description = mdDoc "Enable NFS for downloads volume";
              type = types.bool;
              default = false;
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

          useProbes = mkOption {
            description = mdDoc "Enable readiness and liveness probes";
            type = types.bool;
            default = true;
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
                            name = "RADARR__POSTGRES__HOST";
                            value = cfg.database.host;
                          }
                          {
                            name = "RADARR__POSTGRES__PORT";
                            value = toString cfg.database.port;
                          }
                          {
                            name = "RADARR__POSTGRES__MAINDB";
                            value = cfg.database.name;
                          }
                          {
                            name = "RADARR__POSTGRES__LOGDB";
                            value =
                              if lib.hasSuffix "-main" cfg.database.name then
                                lib.removeSuffix "-main" cfg.database.name + "-log"
                              else
                                "${cfg.database.name}-log";
                          }
                          {
                            name = "RADARR__POSTGRES__USER";
                            value = cfg.database.username;
                          }
                          (
                            if cfg.database.password != "" then
                              {
                                name = "RADARR__POSTGRES__PASSWORD";
                                valueFrom = {
                                  secretKeyRef = {
                                    name = password-secret;
                                    key = "password";
                                  };
                                };
                              }
                            else
                              {
                                name = "RADARR__POSTGRES__PASSWORD";
                                value = "";
                              }
                          )
                        ])
                        ++ (lib.optionals cfg.vpn.enable [
                          # Configure Radarr to use shared gluetun's HTTP proxy
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
                            value = "localhost,127.0.0.1,.svc,.svc.cluster.local,sabnzbd.sabnzbd,sabnzbd.sabnzbd.svc.cluster.local";
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
                            path = "/ping";
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
                            path = "/ping";
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
                          {
                            mountPath = "/downloads";
                            name = "downloads";
                          }
                          {
                            mountPath = "/movies";
                            name = "movies";
                          }
                        ];
                      }
                    ];

                    serviceAccountName = "default";
                    initContainers = lib.optionalAttrs cfg.vpn.enable (
                      self.lib.waitForGluetun { inherit lib; } cfg.vpn.sharedGluetunService
                    );

                    volumes = [
                      cfg.volumes.config.volume
                    ]
                    ++ (lib.optionals (cfg.database.enable && cfg.database.password != "") [
                      {
                        name = password-secret;
                        secret.secretName = password-secret;
                      }
                    ])
                    ++ [
                      {
                        name = "downloads";
                        persistentVolumeClaim.claimName = "${name}-${name}-downloads";
                      }
                      {
                        name = "movies";
                        persistentVolumeClaim.claimName = "${name}-${name}-movies";
                      }
                    ];
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
            "${name}-${name}-downloads".spec =
              if cfg.nfs.enable then
                {
                  accessModes = [ "ReadWriteMany" ];
                  resources.requests.storage = "1Gi";
                  storageClassName = "";
                  volumeName = "${name}-${name}-downloads-nfs";
                }
              else
                {
                  inherit (cfg) storageClassName;
                  accessModes = [ "ReadWriteOnce" ];
                  resources.requests.storage = "50Gi";
                };
            "${name}-${name}-movies".spec =
              if cfg.nfs.enable then
                {
                  accessModes = [ "ReadWriteMany" ];
                  resources.requests.storage = "1Gi";
                  storageClassName = "";
                  volumeName = "${name}-${name}-movies-nfs";
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

          # Create NFS PersistentVolumes for downloads and movies when NFS is enabled
          persistentVolumes = lib.optionalAttrs (cfg.nfs.enable) {
            "${name}-${name}-downloads-nfs" = {
              apiVersion = "v1";
              kind = "PersistentVolume";
              metadata = {
                name = "${name}-${name}-downloads-nfs";
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
                  path = "${cfg.nfs.path}/Downloads";
                };
                persistentVolumeReclaimPolicy = "Retain";
              };
            };
            "${name}-${name}-movies-nfs" = {
              apiVersion = "v1";
              kind = "PersistentVolume";
              metadata = {
                name = "${name}-${name}-movies-nfs";
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
                  path = "${cfg.nfs.path}/Movies";
                };
                persistentVolumeReclaimPolicy = "Retain";
              };
            };
          };
        };
      };
}
