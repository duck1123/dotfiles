{ ... }:
{
  flake.nixidyApps.romm =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      password-secret = "romm-database-password";
      admin-secret = "romm-admin-password";
      metadata-secret = "romm-metadata-secrets";
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
        name = "romm";
        uses-ingress = true;
        uses-database = true;

        # Shape only -- no volumeHandle here, that's environment-specific (see
        # env/dev/romm.nix and docs/pinned-volumes.md).
        volumes = cfg: {
          data.size = "5Gi";
          config.size = "1Gi";
        };

        sopsSecrets =
          cfg:
          {
            ${password-secret} = {
              password = cfg.database.password;
            };
            ${admin-secret} = {
              username = cfg.admin.username;
              password = cfg.admin.password;
              authSecretKey = cfg.authSecretKey;
            };
          }
          //
            lib.optionalAttrs
              (
                cfg.metadata.igdb.enable
                || cfg.metadata.mobygames.enable
                || cfg.metadata.steamgriddb.enable
                || cfg.metadata.screenscraper.enable
              )
              {
                ${metadata-secret} =
                  lib.optionalAttrs cfg.metadata.igdb.enable {
                    igdbClientId = cfg.metadata.igdb.clientId;
                    igdbClientSecret = cfg.metadata.igdb.clientSecret;
                  }
                  // lib.optionalAttrs cfg.metadata.mobygames.enable {
                    mobygamesApiKey = cfg.metadata.mobygames.apiKey;
                  }
                  // lib.optionalAttrs cfg.metadata.steamgriddb.enable {
                    steamgriddbApiKey = cfg.metadata.steamgriddb.apiKey;
                  }
                  // lib.optionalAttrs cfg.metadata.screenscraper.enable {
                    screenscrapeUser = cfg.metadata.screenscraper.username;
                    screenscrapePassword = cfg.metadata.screenscraper.password;
                  };
              };

        extraOptions = {
          image = mkOption {
            description = mdDoc ''
              Docker image to use. Pin a release tag (not `:latest`) so Alembic migration files
              match `alembic_version` in MariaDB; otherwise errors like
              `Can't locate revision identified by '0058_…'` occur when the DB was migrated
              with a newer build than the image ships.
            '';
            type = types.str;
            default = "ghcr.io/rommapp/romm:5.2.0";
          };

          admin = {
            username = mkOption {
              description = mdDoc "The admin username";
              type = types.str;
              default = "admin";
            };

            password = mkOption {
              description = mdDoc "The admin password";
              type = types.str;
              default = "CHANGEME";
            };
          };

          authSecretKey = mkOption {
            description = mdDoc "The authentication secret key (used for sessions)";
            type = types.str;
            default = "CHANGEME";
          };

          nfs = {
            enable = mkOption {
              description = mdDoc "Enable NFS for roms volumes";
              type = types.bool;
              default = false;
            };

            server = mkOption {
              description = mdDoc "NFS server hostname/IP";
              type = types.str;
              default = "nasnix";
            };

            libraryPath = mkOption {
              description = mdDoc "NFS server path for ROM library (where ROM files are stored)";
              type = types.str;
              default = "/mnt/roms";
            };

            assetsPath = mkOption {
              description = mdDoc "NFS server path for assets (metadata, covers, screenshots)";
              type = types.str;
              default = "/mnt/roms/assets";
            };

            resourcesPath = mkOption {
              description = mdDoc "NFS server path for resources (downloaded resources, templates, etc.)";
              type = types.str;
              default = "/mnt/roms/resources";
            };
          };

          service = {
            port = mkOption {
              description = mdDoc "The service port";
              type = types.int;
              default = 5000;
            };
          };

          metadata = {
            igdb = {
              enable = mkOption {
                description = mdDoc ''
                  Enable IGDB metadata source.
                  Requires a Twitch Developer app: https://dev.twitch.tv/console
                  Create an app with OAuth redirect URL https://id.twitch.tv/oauth2/token, then copy the Client ID and generate a Client Secret.
                '';
                type = types.bool;
                default = false;
              };
              clientId = mkOption {
                description = mdDoc "IGDB/Twitch OAuth Client ID";
                type = types.str;
                default = "";
              };
              clientSecret = mkOption {
                description = mdDoc "IGDB/Twitch OAuth Client Secret";
                type = types.str;
                default = "";
              };
            };

            mobygames = {
              enable = mkOption {
                description = mdDoc ''
                  Enable MobyGames metadata source.
                  API key available at https://www.mobygames.com/info/api/ (free tier has rate limits).
                '';
                type = types.bool;
                default = false;
              };
              apiKey = mkOption {
                description = mdDoc "MobyGames API key";
                type = types.str;
                default = "";
              };
            };

            steamgriddb = {
              enable = mkOption {
                description = mdDoc ''
                  Enable SteamGridDB artwork source (box art, banners, icons, logos).
                  API key available at https://www.steamgriddb.com/profile/preferences/api (free account required).
                '';
                type = types.bool;
                default = false;
              };
              apiKey = mkOption {
                description = mdDoc "SteamGridDB API key";
                type = types.str;
                default = "";
              };
            };

            screenscraper = {
              enable = mkOption {
                description = mdDoc ''
                  Enable ScreenScraper metadata source (covers, screenshots, videos, ratings).
                  Free account at https://www.screenscraper.fr/ — no API key, just username/password.
                '';
                type = types.bool;
                default = false;
              };
              username = mkOption {
                description = mdDoc "ScreenScraper username";
                type = types.str;
                default = "";
              };
              password = mkOption {
                description = mdDoc "ScreenScraper password";
                type = types.str;
                default = "";
              };
            };
          };

        };

        extraResources = cfg: {
          deployments.${name} = {
            metadata.labels = {
              "app.kubernetes.io/instance" = name;
              "app.kubernetes.io/name" = name;
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
                        # Try DB_* format (without DATABASE_ prefix)
                        {
                          name = "DB_HOST";
                          value = cfg.database.host;
                        }
                        {
                          name = "DB_PORT";
                          value = "${toString cfg.database.port}";
                        }
                        {
                          name = "DB_NAME";
                          value = cfg.database.name;
                        }
                        {
                          name = "DB_USER";
                          value = cfg.database.username;
                        }
                        {
                          name = "DB_PASSWD";
                          valueFrom.secretKeyRef = {
                            name = password-secret;
                            key = "password";
                          };
                        }
                        # Also provide DATABASE_* format as fallback
                        {
                          name = "DATABASE_HOST";
                          value = cfg.database.host;
                        }
                        {
                          name = "DATABASE_PORT";
                          value = "${toString cfg.database.port}";
                        }
                        {
                          name = "DATABASE_NAME";
                          value = cfg.database.name;
                        }
                        {
                          name = "DATABASE_USER";
                          value = cfg.database.username;
                        }
                        {
                          name = "DATABASE_PASSWORD";
                          valueFrom.secretKeyRef = {
                            name = password-secret;
                            key = "password";
                          };
                        }
                        {
                          name = "ROMM_AUTH_SECRET_KEY";
                          valueFrom.secretKeyRef = {
                            name = admin-secret;
                            key = "authSecretKey";
                          };
                        }
                        {
                          name = "ROMM_ADMIN_USERNAME";
                          valueFrom.secretKeyRef = {
                            name = admin-secret;
                            key = "username";
                          };
                        }
                        {
                          name = "ROMM_ADMIN_PASSWORD";
                          valueFrom.secretKeyRef = {
                            name = admin-secret;
                            key = "password";
                          };
                        }
                        {
                          name = "ROMM_CONFIG_PATH";
                          value = "/romm/config/config.yml";
                        }
                        # Nginx bind configuration
                        # Nginx should listen on 8080, gunicorn runs on 5000
                        # Based on Dockerfile: EXPOSE 8080 6379/tcp
                        # The nginx template likely uses PORT for the listen directive
                        {
                          name = "HOST";
                          value = "0.0.0.0";
                        }
                        {
                          name = "PORT";
                          value = "8080";
                        }
                        {
                          name = "ROMM_HOST";
                          value = "0.0.0.0";
                        }
                        {
                          name = "ROMM_PORT";
                          value = "8080";
                        }
                        {
                          name = "NGINX_PORT";
                          value = "8080";
                        }
                        {
                          name = "GUNICORN_PORT";
                          value = "${toString cfg.service.port}";
                        }
                        {
                          name = "GUNICORN_HOST";
                          value = "127.0.0.1";
                        }
                        # Valkey/Redis configuration - romm uses internal valkey by default
                        # If you want to use external Redis, uncomment and configure:
                        # {
                        #   name = "REDIS_HOST";
                        #   value = "redis.redis";
                        # }
                        # {
                        #   name = "REDIS_PORT";
                        #   value = "6379";
                        # }
                      ]
                      ++ lib.optionals cfg.metadata.igdb.enable [
                        {
                          name = "IGDB_CLIENT_ID";
                          valueFrom.secretKeyRef = {
                            name = metadata-secret;
                            key = "igdbClientId";
                          };
                        }
                        {
                          name = "IGDB_CLIENT_SECRET";
                          valueFrom.secretKeyRef = {
                            name = metadata-secret;
                            key = "igdbClientSecret";
                          };
                        }
                      ]
                      ++ lib.optionals cfg.metadata.mobygames.enable [
                        {
                          name = "MOBYGAMES_API_KEY";
                          valueFrom.secretKeyRef = {
                            name = metadata-secret;
                            key = "mobygamesApiKey";
                          };
                        }
                      ]
                      ++ lib.optionals cfg.metadata.steamgriddb.enable [
                        {
                          name = "STEAMGRIDDB_API_KEY";
                          valueFrom.secretKeyRef = {
                            name = metadata-secret;
                            key = "steamgriddbApiKey";
                          };
                        }
                      ]
                      ++ lib.optionals cfg.metadata.screenscraper.enable [
                        {
                          name = "SCREENSCRAPER_USER";
                          valueFrom.secretKeyRef = {
                            name = metadata-secret;
                            key = "screenscrapeUser";
                          };
                        }
                        {
                          name = "SCREENSCRAPER_PASSWORD";
                          valueFrom.secretKeyRef = {
                            name = metadata-secret;
                            key = "screenscrapePassword";
                          };
                        }
                      ];

                      ports = [
                        {
                          containerPort = 8080;
                          name = "http";
                          protocol = "TCP";
                        }
                        {
                          containerPort = 6379;
                          name = "redis";
                          protocol = "TCP";
                        }
                      ];

                      volumeMounts = [
                        {
                          mountPath = "/romm/data";
                          name = "data";
                        }
                        {
                          mountPath = "/romm/config";
                          name = "config";
                        }
                        {
                          mountPath = "/romm/library";
                          name = "library";
                        }
                        {
                          mountPath = "/romm/assets";
                          name = "assets";
                        }
                        {
                          mountPath = "/romm/resources";
                          name = "resources";
                        }
                      ];
                    }
                  ];
                  volumes = [
                    cfg.volumes.data.volume
                    cfg.volumes.config.volume
                    {
                      name = "library";
                      persistentVolumeClaim.claimName = "${name}-library";
                    }
                    {
                      name = "assets";
                      persistentVolumeClaim.claimName = "${name}-assets";
                    }
                    {
                      name = "resources";
                      persistentVolumeClaim.claimName = "${name}-resources";
                    }
                  ];
                  # Init container to pre-create config file
                  initContainers = [
                    {
                      name = "init-config";
                      image = "busybox:latest";
                      command = [
                        "sh"
                        "-c"
                        ''
                          if [ ! -f /romm/config/config.yml ]; then
                            echo "Creating initial config.yml file"
                            touch /romm/config/config.yml
                            chmod 644 /romm/config/config.yml
                          else
                            echo "config.yml already exists"
                          fi
                        ''
                      ];
                      volumeMounts = [
                        {
                          mountPath = "/romm/config";
                          name = "config";
                        }
                      ];
                    }
                    {
                      # The Synology NFS export applies Windows ACLs that can deny
                      # read access to uid=1000 (romm, the nginx worker user) even
                      # when POSIX permissions show 777. Running chmod as root via
                      # NFS resets the ACLs to match POSIX, restoring access.
                      name = "fix-library-permissions";
                      image = "busybox:latest";
                      command = [
                        "sh"
                        "-c"
                        "chmod -R a+r /romm/library/ && echo 'Library permissions fixed'"
                      ];
                      volumeMounts = [
                        {
                          mountPath = "/romm/library";
                          name = "library";
                        }
                      ];
                    }
                  ];
                };
              };
            };
          };

          services.${name}.spec = {
            ports = [
              {
                name = "http";
                port = 8080;
                protocol = "TCP";
                targetPort = "http";
              }
              {
                name = "redis";
                port = 6379;
                protocol = "TCP";
                targetPort = "redis";
              }
            ];

            selector = {
              "app.kubernetes.io/instance" = name;
              "app.kubernetes.io/name" = name;
            };
            type = "ClusterIP";
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
            "${name}-library".spec =
              if cfg.nfs.enable then
                {
                  accessModes = [ "ReadWriteMany" ];
                  resources.requests.storage = "1Gi";
                  storageClassName = "";
                  volumeName = "${name}-${name}-library-nfs";
                }
              else
                {
                  accessModes = [ "ReadWriteOnce" ];
                  resources.requests.storage = "5Gi";
                  storageClassName = cfg.storageClassName;
                };
            "${name}-assets".spec =
              if cfg.nfs.enable then
                {
                  accessModes = [ "ReadWriteMany" ];
                  resources.requests.storage = "1Gi";
                  storageClassName = "";
                  volumeName = "${name}-${name}-assets-nfs";
                }
              else
                {
                  accessModes = [ "ReadWriteOnce" ];
                  resources.requests.storage = "5Gi";
                  storageClassName = cfg.storageClassName;
                };
            "${name}-resources".spec =
              if cfg.nfs.enable then
                {
                  accessModes = [ "ReadWriteMany" ];
                  resources.requests.storage = "1Gi";
                  storageClassName = "";
                  volumeName = "${name}-${name}-resources-nfs";
                }
              else
                {
                  accessModes = [ "ReadWriteOnce" ];
                  resources.requests.storage = "5Gi";
                  storageClassName = cfg.storageClassName;
                };
          };

          # Create NFS PersistentVolumes for roms when NFS is enabled
          persistentVolumes = lib.optionalAttrs cfg.nfs.enable {
            "${name}-${name}-library-nfs" = {
              apiVersion = "v1";
              kind = "PersistentVolume";
              metadata = {
                name = "${name}-${name}-library-nfs";
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
                  path = cfg.nfs.libraryPath;
                };
                persistentVolumeReclaimPolicy = "Retain";
              };
            };
            "${name}-${name}-assets-nfs" = {
              apiVersion = "v1";
              kind = "PersistentVolume";
              metadata = {
                name = "${name}-${name}-assets-nfs";
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
                  path = cfg.nfs.assetsPath;
                };
                persistentVolumeReclaimPolicy = "Retain";
              };
            };
            "${name}-${name}-resources-nfs" = {
              apiVersion = "v1";
              kind = "PersistentVolume";
              metadata = {
                name = "${name}-${name}-resources-nfs";
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
                  path = cfg.nfs.resourcesPath;
                };
                persistentVolumeReclaimPolicy = "Retain";
              };
            };
          };

        };
      };
}
