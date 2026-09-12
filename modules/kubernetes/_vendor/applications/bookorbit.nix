{ ... }:
{
  flake.nixidyApps.bookorbit =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      name = "bookorbit";
      jwt-secret = "bookorbit-jwt-secret";
      setup-token-secret = "bookorbit-setup-token";
      db-secret = "bookorbit-database-password";
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
        inherit name;
        uses-ingress = true;
        uses-nfs = true;
        uses-database = true;

        # Shape only -- no volumeHandle here, that's environment-specific (see
        # env/dev/bookorbit.nix and docs/pinned-volumes.md).
        volumes = cfg: {
          data = {
            size = "20Gi";
            volumeAttributes.backupTargetName = "default";
          };
        };

        sopsSecrets = cfg: {
          ${jwt-secret}.JWT_SECRET = cfg.jwtSecret;
          ${setup-token-secret}.SETUP_BOOTSTRAP_TOKEN = cfg.setupBootstrapToken;
          ${db-secret}.password = cfg.database.password;
        };

        extraOptions = {
          image = mkOption {
            description = mdDoc "The docker image";
            type = types.str;
            default = "ghcr.io/bookorbit/bookorbit:2.9.0";
          };

          service.port = mkOption {
            description = mdDoc "The service port";
            type = types.int;
            default = 3000;
          };

          puid = mkOption {
            description = mdDoc "Container runtime UID for files written to app-managed data folders";
            type = types.int;
            default = 1000;
          };

          pgid = mkOption {
            description = mdDoc "Container runtime GID for files written to app-managed data folders";
            type = types.int;
            default = 1000;
          };

          nodeMaxOldSpaceSize = mkOption {
            description = mdDoc "Node.js JavaScript heap limit in MB -- raise for very large libraries";
            type = types.str;
            default = "2048";
          };

          jwtSecret = mkOption {
            description = mdDoc "JWT signing secret (generate: openssl rand -hex 32). Stored in secrets.enc.yaml as `bookorbit.jwtSecret`.";
            type = types.str;
            default = "";
          };

          setupBootstrapToken = mkOption {
            description = mdDoc ''
              One-time token required by the `/auth/setup` endpoint to create the first
              admin account. Stored in secrets.enc.yaml as `bookorbit.setupBootstrapToken`
              (generate: openssl rand -hex 32).
            '';
            type = types.str;
            default = "";
          };

          nfs.audiobooksPath = mkOption {
            description = mdDoc ''
              NFS export path (on the same `nfs.server`) for a second library, mounted at
              /books/Audiobooks alongside the primary `nfs.path` export. Leave empty to
              skip mounting a second export. Environment-specific -- set in
              env/dev/bookorbit.nix, not here.
            '';
            type = types.str;
            default = "";
          };
        };

        extraResources = cfg: {
          deployments.${name} = {
            metadata.labels = {
              "app.kubernetes.io/instance" = name;
              "app.kubernetes.io/name" = name;
              "app.kubernetes.io/version" = "2.9.0";
            };

            spec = {
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

                  containers = [
                    {
                      inherit name;
                      image = cfg.image;
                      imagePullPolicy = "IfNotPresent";
                      env = [
                        {
                          name = "NODE_ENV";
                          value = "production";
                        }
                        {
                          name = "PORT";
                          value = toString cfg.service.port;
                        }
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
                        {
                          name = "NODE_MAX_OLD_SPACE_SIZE";
                          value = cfg.nodeMaxOldSpaceSize;
                        }
                        {
                          name = "POSTGRES_HOST";
                          value = cfg.database.host;
                        }
                        {
                          name = "POSTGRES_PORT";
                          value = toString cfg.database.port;
                        }
                        {
                          name = "POSTGRES_USER";
                          value = cfg.database.username;
                        }
                        {
                          name = "POSTGRES_DB";
                          value = cfg.database.name;
                        }
                        {
                          name = "POSTGRES_PASSWORD";
                          valueFrom.secretKeyRef = {
                            name = db-secret;
                            key = "password";
                          };
                        }
                        {
                          name = "JWT_SECRET";
                          valueFrom.secretKeyRef = {
                            name = jwt-secret;
                            key = "JWT_SECRET";
                          };
                        }
                        {
                          name = "SETUP_BOOTSTRAP_TOKEN";
                          valueFrom.secretKeyRef = {
                            name = setup-token-secret;
                            key = "SETUP_BOOTSTRAP_TOKEN";
                          };
                        }
                        {
                          name = "APP_URL";
                          value = "https://${cfg.ingress.domain}";
                        }
                        {
                          name = "LIBRARY_BROWSE_ROOT";
                          value = "/books";
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
                        tcpSocket.port = "http";
                        initialDelaySeconds = 15;
                        periodSeconds = 10;
                        timeoutSeconds = 5;
                        successThreshold = 1;
                        failureThreshold = 5;
                      };

                      livenessProbe = {
                        tcpSocket.port = "http";
                        initialDelaySeconds = 30;
                        periodSeconds = 30;
                        timeoutSeconds = 5;
                        successThreshold = 1;
                        failureThreshold = 5;
                      };

                      volumeMounts = [
                        {
                          mountPath = "/books";
                          name = "books";
                        }
                        {
                          mountPath = "/data";
                          name = "data";
                        }
                      ]
                      ++ optional (cfg.nfs.enable && cfg.nfs.audiobooksPath != "") {
                        mountPath = "/books/Audiobooks";
                        name = "audiobooks";
                      };
                    }
                  ];

                  volumes = [
                    {
                      name = "books";
                      persistentVolumeClaim.claimName = "${name}-${name}-books";
                    }
                    cfg.volumes.data.volume
                  ]
                  ++ optional (cfg.nfs.enable && cfg.nfs.audiobooksPath != "") {
                    name = "audiobooks";
                    persistentVolumeClaim.claimName = "${name}-${name}-audiobooks";
                  };
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

          persistentVolumeClaims =
            {
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
                    resources.requests.storage = "100Gi";
                  };
            }
            // optionalAttrs (cfg.nfs.enable && cfg.nfs.audiobooksPath != "") {
              "${name}-${name}-audiobooks".spec = {
                accessModes = [ "ReadWriteMany" ];
                resources.requests.storage = "1Gi";
                storageClassName = "";
                volumeName = "${name}-${name}-audiobooks-nfs";
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

          persistentVolumes =
            lib.optionalAttrs cfg.nfs.enable {
              "${name}-${name}-books-nfs" = {
                apiVersion = "v1";
                kind = "PersistentVolume";
                metadata = {
                  name = "${name}-${name}-books-nfs";
                };
                spec = {
                  capacity.storage = "1Ti";
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
            }
            // lib.optionalAttrs (cfg.nfs.enable && cfg.nfs.audiobooksPath != "") {
              "${name}-${name}-audiobooks-nfs" = {
                apiVersion = "v1";
                kind = "PersistentVolume";
                metadata = {
                  name = "${name}-${name}-audiobooks-nfs";
                };
                spec = {
                  capacity.storage = "1Ti";
                  accessModes = [ "ReadWriteMany" ];
                  mountOptions = [
                    "nolock"
                    "noexec"
                    "soft"
                    "timeo=30"
                  ];
                  nfs = {
                    server = cfg.nfs.server;
                    path = cfg.nfs.audiobooksPath;
                  };
                  persistentVolumeReclaimPolicy = "Retain";
                };
              };
            };
        };
      };
}
