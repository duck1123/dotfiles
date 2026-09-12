{ ... }:
{
  flake.nixidyApps.booklore =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
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
        name = "booklore";
        uses-ingress = true;
        uses-nfs = true;
        uses-database = true;

        sopsSecrets = cfg: {
          "booklore-database-password" = {
            password = cfg.database.password;
          };
        };

        # Shape only -- no volumeHandle here, that's environment-specific (see
        # env/dev/booklore.nix and docs/pinned-volumes.md).
        volumes = cfg: {
          data = {
            size = "5Gi";
            volumeAttributes.backupTargetName = "default";
          };
          bookdrop = {
            size = "5Gi";
            volumeAttributes.backupTargetName = "default";
          };
        };

        extraOptions = {
          gid = mkOption {
            description = mdDoc "The group id";
            type = types.str;
            default = "1000";
          };

          image = mkOption {
            description = mdDoc "The docker image";
            type = types.str;
            default = "booklore/booklore:latest";
          };

          service.port = mkOption {
            description = mdDoc "The service port";
            type = types.int;
            default = 6060;
          };

          uid = mkOption {
            description = mdDoc "The user id";
            type = types.str;
            default = "1000";
          };

        };

        extraResources = cfg: {
          deployments = {
            booklore = {
              metadata.labels = {
                "app.kubernetes.io/instance" = name;
                "app.kubernetes.io/name" = name;
                "app.kubernetes.io/version" = "0.8.7";
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
                            name = "USER_ID";
                            value = cfg.uid;
                          }
                          {
                            name = "GROUP_ID";
                            value = cfg.gid;
                          }
                          {
                            name = "TZ";
                            value = cfg.tz;
                          }
                          {
                            name = "DATABASE_URL";
                            value = "jdbc:mariadb://${cfg.database.host}:${toString cfg.database.port}/${cfg.database.name}";
                          }
                          {
                            name = "DATABASE_USERNAME";
                            value = cfg.database.username;
                          }
                          {
                            name = "DATABASE_PASSWORD";
                            valueFrom.secretKeyRef = {
                              name = "booklore-database-password";
                              key = "password";
                            };
                          }
                          {
                            name = "BOOKLORE_PORT";
                            value = "${toString cfg.service.port}";
                          }
                          {
                            name = "SWAGGER_ENABLED";
                            value = "false";
                          }
                        ];

                        livenessProbe = {
                          failureThreshold = 3;
                          initialDelaySeconds = 0;
                          periodSeconds = 10;
                          tcpSocket.port = 6060;
                        };

                        ports = [
                          {
                            containerPort = 6060;
                            name = "http";
                            protocol = "TCP";
                          }
                        ];

                        volumeMounts = [
                          {
                            mountPath = "/bookdrop";
                            name = "bookdrop";
                          }
                          {
                            mountPath = "/books";
                            name = "books";
                          }
                          {
                            mountPath = "/app/data";
                            name = "data";
                          }
                        ];
                      }
                    ];
                    volumes = [
                      cfg.volumes.bookdrop.volume
                      {
                        name = "books";
                        persistentVolumeClaim.claimName = "${name}-${name}-books";
                      }
                      cfg.volumes.data.volume
                    ];
                  };
                };
              };
            };
          };

          ingresses = {
            ${name} = with cfg.ingress; {
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
          };

          persistentVolumeClaims = {
            "${name}-${name}-books".spec =
              if cfg.nfs.enable then
                {
                  accessModes = [ "ReadWriteMany" ];
                  resources.requests.storage = "1Gi";
                  storageClassName = "";
                  volumeName = "${name}-${name}-books-nfs";
                }
              else
                {
                  inherit (cfg) storageClassName;
                  accessModes = [ "ReadWriteOnce" ];
                  resources.requests.storage = "5Gi";
                };
          };

          services = {
            ${name}.spec = {
              ports = [
                {
                  name = "http";
                  port = 6060;
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

          # data/bookdrop are pinned volumes -- see the `pinnedVolumes` attrset
          # above; their PersistentVolume/PersistentVolumeClaim resources are
          # merged in automatically by mkArgoApp.
          persistentVolumes = lib.optionalAttrs cfg.nfs.enable {
            "${name}-${name}-books-nfs" = {
              apiVersion = "v1";
              kind = "PersistentVolume";
              metadata = {
                name = "${name}-${name}-books-nfs";
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
