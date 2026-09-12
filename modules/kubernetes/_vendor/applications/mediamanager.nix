{ ... }:
{
  flake.nixidyApps.mediamanager =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      name = "mediamanager";
      db-secret = "mediamanager-database";
      auth-secret = "mediamanager-auth";

      # Non-secret app config. Postgres credentials and the auth token secret
      # are intentionally left out -- they're injected via MEDIAMANAGER_* env
      # vars from sops secrets instead (pydantic-settings' env source wins
      # over the TOML source), so they never sit in a plaintext ConfigMap.
      configToml = cfg: ''
        [misc]
        frontend_url = "https://${cfg.ingress.domain}"
        cors_urls = ["https://${cfg.ingress.domain}"]
        image_directory = "/data/images"
        tv_directory = "/data/tv"
        movie_directory = "/data/movies"
        torrent_directory = "/data/torrents"
        development = false

        [[misc.tv_libraries]]
        name = "TV"
        path = "/data/tv"

        [[misc.movie_libraries]]
        name = "Movies"
        path = "/data/movies"

        [auth]
        # No registration_enabled here -- that field doesn't exist yet in the
        # pinned 1.12.3 AuthConfig (pydantic-settings' extra="forbid" default
        # rejects unknown keys outright); it's a later-release addition.
        admin_emails = [${concatStringsSep ", " (map (e: ''"${e}"'') cfg.adminEmails)}]
      '';
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
        # env/dev/mediamanager.nix and docs/pinned-volumes.md).
        volumes = cfg: {
          images.size = "5Gi";
          config.size = "500Mi";
        };

        extraOptions = {
          image = mkOption {
            description = mdDoc "Docker image to use (combined frontend+API image). Pin a release tag, not `:latest`.";
            type = types.str;
            default = "quay.io/maxdorninger/mediamanager:1.12.3";
          };

          service.port = mkOption {
            description = mdDoc "The service port";
            type = types.int;
            default = 8000;
          };

          puid = mkOption {
            description = mdDoc "The user ID to run the container as. The image's entrypoint runs as root and chowns everything under /app/config to this uid on startup -- pointless (and noisy, since /app/config is a read-only ConfigMap mount here) but harmless when it matches, since pod securityContext.runAsUser already puts the process itself at this uid.";
            type = types.int;
            default = 1000;
          };

          pgid = mkOption {
            description = mdDoc "The group ID to run the container as.";
            type = types.int;
            default = 1000;
          };

          tokenSecret = mkOption {
            description = mdDoc "Value for auth.token_secret (session/JWT signing key -- generate with `openssl rand -hex 32`). Empty = the app falls back to a fresh random secret on every restart, invalidating all sessions.";
            type = types.str;
            default = "";
          };

          adminEmails = mkOption {
            description = mdDoc "Emails that become administrators on registration; if no users exist yet, the first entry is auto-created as the default admin.";
            type = types.listOf types.str;
            default = [ ];
          };
        };

        sopsSecrets =
          cfg:
          optionalAttrs (cfg.database.password != "") {
            ${db-secret}.password = cfg.database.password;
          }
          // optionalAttrs (cfg.tokenSecret != "") {
            ${auth-secret}.tokenSecret = cfg.tokenSecret;
          };

        extraResources =
          cfg:
          let
            labels = {
              "app.kubernetes.io/instance" = name;
              "app.kubernetes.io/name" = name;
            };
          in
          {
            configMaps.${name}.data."config.toml" = configToml cfg;

            deployments.${name} = {
              metadata.labels = labels;

              spec = {
                replicas = 1;
                selector.matchLabels = labels;

                template = {
                  metadata.labels = labels;

                  spec = {
                    serviceAccountName = "default";
                    securityContext = {
                      runAsUser = cfg.puid;
                      runAsGroup = cfg.pgid;
                      fsGroup = cfg.pgid;
                    };

                    containers = [
                      {
                        inherit name;
                        image = cfg.image;
                        imagePullPolicy = "IfNotPresent";

                        env = [
                          {
                            name = "CONFIG_DIR";
                            value = "/app/config";
                          }
                          {
                            name = "MEDIAMANAGER_DATABASE__HOST";
                            value = cfg.database.host;
                          }
                          {
                            name = "MEDIAMANAGER_DATABASE__PORT";
                            value = toString cfg.database.port;
                          }
                          {
                            name = "MEDIAMANAGER_DATABASE__USER";
                            value = cfg.database.username;
                          }
                          {
                            name = "MEDIAMANAGER_DATABASE__DBNAME";
                            value = cfg.database.name;
                          }
                        ]
                        ++ (
                          if cfg.database.password != "" then
                            [
                              {
                                name = "MEDIAMANAGER_DATABASE__PASSWORD";
                                valueFrom.secretKeyRef = {
                                  name = db-secret;
                                  key = "password";
                                };
                              }
                            ]
                          else
                            [ ]
                        )
                        ++ optional (cfg.tokenSecret != "") {
                          name = "MEDIAMANAGER_AUTH__TOKEN_SECRET";
                          valueFrom.secretKeyRef = {
                            name = auth-secret;
                            key = "tokenSecret";
                          };
                        };

                        ports = [
                          {
                            containerPort = cfg.service.port;
                            name = "http";
                            protocol = "TCP";
                          }
                        ];

                        readinessProbe = {
                          httpGet = {
                            path = "/api/v1/health";
                            port = cfg.service.port;
                          };
                          initialDelaySeconds = 20;
                          periodSeconds = 10;
                          timeoutSeconds = 5;
                          failureThreshold = 6;
                        };
                        livenessProbe = {
                          httpGet = {
                            path = "/api/v1/health";
                            port = cfg.service.port;
                          };
                          initialDelaySeconds = 30;
                          periodSeconds = 30;
                          timeoutSeconds = 5;
                          failureThreshold = 3;
                        };

                        volumeMounts = [
                          {
                            mountPath = "/app/config";
                            name = "config";
                          }
                          # Overlay just config.toml from the ConfigMap on top of the
                          # writable PVC above -- the app writes its own log file
                          # (media_manager.log) and possibly other state into
                          # /app/config, so the whole directory can't be a read-only
                          # ConfigMap mount (that's what broke both the root-init
                          # chown step and this file-logging handler).
                          {
                            mountPath = "/app/config/config.toml";
                            name = "config-file";
                            subPath = "config.toml";
                          }
                          {
                            mountPath = "/data/images";
                            name = "images";
                          }
                          {
                            mountPath = "/data/tv";
                            name = "tv";
                          }
                          {
                            mountPath = "/data/movies";
                            name = "movies";
                          }
                          {
                            mountPath = "/data/torrents";
                            name = "torrents";
                          }
                        ];
                      }
                    ];

                    volumes = [
                      cfg.volumes.config.volume
                      {
                        name = "config-file";
                        configMap.name = name;
                      }
                      cfg.volumes.images.volume
                      {
                        name = "tv";
                        persistentVolumeClaim.claimName = "${name}-${name}-tv";
                      }
                      {
                        name = "movies";
                        persistentVolumeClaim.claimName = "${name}-${name}-movies";
                      }
                      {
                        name = "torrents";
                        persistentVolumeClaim.claimName = "${name}-${name}-torrents";
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
                  port = cfg.service.port;
                  protocol = "TCP";
                  targetPort = "http";
                }
              ];
              selector = labels;
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
                    secretName = "${name}-tls";
                  }
                ];
              };
            };

            # Media/downloads: NFS-backed shared-library mounts when cfg.nfs.enable
            # (the usual mode -- shares the same NAS TV/Movies/Downloads folders
            # sonarr/radarr already use), otherwise ordinary per-app PVCs.
            persistentVolumeClaims = {
              "${name}-${name}-tv".spec =
                if cfg.nfs.enable then
                  {
                    accessModes = [ "ReadWriteMany" ];
                    resources.requests.storage = "1Gi";
                    storageClassName = "";
                    volumeName = "${name}-${name}-tv-nfs";
                  }
                else
                  {
                    inherit (cfg) storageClassName;
                    accessModes = [ "ReadWriteOnce" ];
                    resources.requests.storage = "100Gi";
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
              "${name}-${name}-torrents".spec =
                if cfg.nfs.enable then
                  {
                    accessModes = [ "ReadWriteMany" ];
                    resources.requests.storage = "1Gi";
                    storageClassName = "";
                    volumeName = "${name}-${name}-torrents-nfs";
                  }
                else
                  {
                    inherit (cfg) storageClassName;
                    accessModes = [ "ReadWriteOnce" ];
                    resources.requests.storage = "1Gi";
                  };
            };

            persistentVolumes = optionalAttrs cfg.nfs.enable {
              "${name}-${name}-tv-nfs" = {
                apiVersion = "v1";
                kind = "PersistentVolume";
                metadata.name = "${name}-${name}-tv-nfs";
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
                    path = "${cfg.nfs.path}/TV";
                  };
                  persistentVolumeReclaimPolicy = "Retain";
                };
              };
              "${name}-${name}-movies-nfs" = {
                apiVersion = "v1";
                kind = "PersistentVolume";
                metadata.name = "${name}-${name}-movies-nfs";
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
                    path = "${cfg.nfs.path}/Movies";
                  };
                  persistentVolumeReclaimPolicy = "Retain";
                };
              };
              "${name}-${name}-torrents-nfs" = {
                apiVersion = "v1";
                kind = "PersistentVolume";
                metadata.name = "${name}-${name}-torrents-nfs";
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
                    path = "${cfg.nfs.path}/Downloads";
                  };
                  persistentVolumeReclaimPolicy = "Retain";
                };
              };
            };
          };
      };
}
