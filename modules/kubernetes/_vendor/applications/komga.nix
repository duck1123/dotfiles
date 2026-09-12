{ ... }:
{
  flake.nixidyApps.komga =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      cfg = config.services.komga;
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
        name = "komga";
        uses-ingress = true;

        # Shape only -- no volumeHandle here, that's environment-specific (see
        # env/dev/komga.nix and docs/pinned-volumes.md).
        volumes = cfg: {
          config.size = "5Gi";
        };

        extraOptions = {
          gid = mkOption {
            description = mdDoc "The group id";
            type = types.str;
            default = "1000";
          };

          apiKey = mkOption {
            description = mdDoc ''
              Komga API key (web UI -> Settings -> Users -> API Keys). Stored in
              secrets.enc.yaml as `komga.key` and wired in via
              `env/dev/komga.nix`. Only powers the auto-added homepage dashboard
              widget below -- never injected into the komga container itself.
            '';
            type = types.str;
            default = "";
          };

          # Auto-add a Komga widget to this app's homepage dashboard tile once
          # an API key is configured -- see applications/immich.nix for the same
          # pattern with more detail. Set
          # `services.homepage.widgetSecrets.KOMGA_API_KEY` from
          # `config.services.komga.apiKey` in env/dev/homepage.nix. Add
          # `version = 2;` here if this ever moves to Komga v2.
          homepage.extraSettings = mkOption {
            default = lib.optionalAttrs (cfg.apiKey != "") {
              widget = {
                type = "komga";
                url = "http://${name}.${cfg.namespace}:${toString cfg.service.port}";
                key = "{{HOMEPAGE_VAR_KOMGA_API_KEY}}";
              };
            };
          };

          image = mkOption {
            description = mdDoc "The docker image";
            type = types.str;
            default = "gotson/komga:latest";
          };

          service.port = mkOption {
            description = mdDoc "The service port";
            type = types.int;
            default = 25600;
          };

          nfs = {
            enable = mkOption {
              description = mdDoc "Enable NFS for data volume";
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
              default = "/mnt/comics";
            };
          };

          uid = mkOption {
            description = mdDoc "The user id";
            type = types.str;
            default = "1000";
          };

        };

        extraResources = cfg: {
          deployments = {
            komga = {
              metadata.labels = {
                "app.kubernetes.io/instance" = name;
                "app.kubernetes.io/name" = name;
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
                            name = "PUID";
                            value = cfg.uid;
                          }
                          {
                            name = "PGID";
                            value = cfg.gid;
                          }
                          {
                            name = "TZ";
                            value = cfg.tz;
                          }
                          {
                            name = "SERVER_PORT";
                            value = "${toString cfg.service.port}";
                          }
                        ];

                        livenessProbe = {
                          failureThreshold = 3;
                          initialDelaySeconds = 30;
                          periodSeconds = 10;
                          tcpSocket.port = cfg.service.port;
                        };

                        ports = [
                          {
                            containerPort = cfg.service.port;
                            name = "http";
                            protocol = "TCP";
                          }
                        ];

                        volumeMounts = [
                          {
                            mountPath = "/config";
                            name = "config";
                          }
                          {
                            mountPath = "/data";
                            name = "data";
                          }
                        ];
                      }
                    ];
                    volumes = [
                      cfg.volumes.config.volume
                      {
                        name = "data";
                        persistentVolumeClaim.claimName = "${name}-${name}-data";
                      }
                    ];
                  };
                };
              };
            };
          };

          ingresses = {
            ${name} = {
              metadata.annotations."cert-manager.io/cluster-issuer" = cfg.ingress.clusterIssuer;
              spec = with cfg.ingress; {
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
          };

          persistentVolumeClaims = {
            "${name}-${name}-data".spec =
              if cfg.nfs.enable then
                {
                  accessModes = [ "ReadWriteMany" ];
                  resources.requests.storage = "1Gi";
                  storageClassName = "";
                  volumeName = "${name}-${name}-data-nfs";
                }
              else
                {
                  inherit (cfg) storageClassName;
                  accessModes = [ "ReadWriteOnce" ];
                  resources.requests.storage = "50Gi";
                };
          };

          services = {
            ${name}.spec = {
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

          persistentVolumes = lib.optionalAttrs cfg.nfs.enable {
            "${name}-${name}-data-nfs" = {
              apiVersion = "v1";
              kind = "PersistentVolume";
              metadata = {
                name = "${name}-${name}-data-nfs";
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
                  path = cfg.nfs.path;
                };
                persistentVolumeReclaimPolicy = "Retain";
              };
            };
          };
        };
      };
}
