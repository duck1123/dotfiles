{ ... }:
{
  flake.nixidyApps.soularr =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      lidarr-api-secret = "soularr-lidarr-api-key";
      slskd-api-secret = "soularr-slskd-api-key";
      # Default config.ini template; [Lidarr] and [Slskd] connection bits are filled from env in init container
      writeConfigScript = ''
        set -e
        cat > /data/config.ini << 'SOULARR_CONFIG'
        [Lidarr]
        api_key = __LIDARR_API_KEY__
        host_url = __LIDARR_HOST_URL__
        download_dir = __LIDARR_DOWNLOAD_DIR__
        disable_sync = False

        [Slskd]
        api_key = __SLSKD_API_KEY__
        host_url = __SLSKD_HOST_URL__
        url_base = /
        download_dir = /downloads
        delete_searches = False
        stalled_timeout = 3600
        remote_queue_timeout = 300

        [Release Settings]
        use_most_common_tracknum = True
        allow_multi_disc = True
        accepted_countries = Europe,Japan,United Kingdom,United States,[Worldwide],Australia,Canada
        skip_region_check = False
        accepted_formats = CD,Digital Media,Vinyl

        [Search Settings]
        search_timeout = 5000
        maximum_peer_queue = 50
        minimum_peer_upload_speed = 0
        minimum_filename_match_ratio = 0.8
        allowed_filetypes = flac 24/192,flac 16/44.1,flac,mp3 320,mp3
        ignored_users = User1,User2,Fred,Bob
        search_for_tracks = True
        album_prepend_artist = False
        track_prepend_artist = True
        search_type = incrementing_page
        number_of_albums_to_grab = 10
        remove_wanted_on_failure = False
        title_blacklist = BlacklistWord1,blacklistword2
        search_blacklist = WordToStripFromSearch1,WordToStripFromSearch2
        search_source = missing
        enable_search_denylist = False
        max_search_failures = 3

        [Download Settings]
        download_filtering = True
        use_extension_whitelist = False
        extensions_whitelist = lrc,nfo,txt

        [Logging]
        level = INFO
        format = [%(levelname)s|%(module)s|L%(lineno)d] %(asctime)s: %(message)s
        datefmt = %Y-%m-%dT%H:%M:%S%z
        SOULARR_CONFIG
        sed -i "s|__LIDARR_API_KEY__|$LIDARR_API_KEY|g" /data/config.ini
        sed -i "s|__LIDARR_HOST_URL__|$LIDARR_HOST_URL|g" /data/config.ini
        sed -i "s|__LIDARR_DOWNLOAD_DIR__|$LIDARR_DOWNLOAD_DIR|g" /data/config.ini
        sed -i "s|__SLSKD_API_KEY__|$SLSKD_API_KEY|g" /data/config.ini
        sed -i "s|__SLSKD_HOST_URL__|$SLSKD_HOST_URL|g" /data/config.ini
        chown -R ''${PUID}:''${PGID} /data
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
        name = "soularr";
        uses-ingress = false;

        # Shape only -- no volumeHandle here, that's environment-specific (see
        # env/dev/soularr.nix and docs/pinned-volumes.md).
        volumes = cfg: {
          config.size = "1Gi";
        };

        sopsSecrets =
          cfg:
          { }
          // lib.optionalAttrs (cfg.lidarr.apiKey != "") {
            ${lidarr-api-secret} = {
              api_key = cfg.lidarr.apiKey;
            };
          }
          // lib.optionalAttrs (cfg.slskd.apiKey != "") {
            ${slskd-api-secret} = {
              api_key = cfg.slskd.apiKey;
            };
          };

        extraOptions = {
          image = mkOption {
            description = mdDoc "Soularr Docker image (Lidarr–Soulseek companion)";
            type = types.str;
            default = "mrusse08/soularr:latest";
          };

          pgid = mkOption {
            description = mdDoc "Group ID for file ownership";
            type = types.int;
            default = 1000;
          };

          puid = mkOption {
            description = mdDoc "User ID for file ownership";
            type = types.int;
            default = 1000;
          };

          scriptInterval = mkOption {
            description = mdDoc "How often (seconds) Soularr runs to sync Lidarr wanted with Slskd";
            type = types.int;
            default = 300;
          };

          nfs = {
            enable = mkOption {
              description = mdDoc "Use NFS for Slskd downloads (must be visible to Lidarr as well)";
              type = types.bool;
              default = false;
            };

            server = mkOption {
              description = mdDoc "NFS server hostname/IP";
              type = types.str;
              default = "nasnix";
            };

            path = mkOption {
              description = mdDoc "NFS path for Slskd downloads (e.g. /volume1/slskd_downloads)";
              type = types.str;
              default = "/mnt/media/slskd_downloads";
            };
          };

          lidarr = {
            host = mkOption {
              description = mdDoc "Lidarr service host (e.g. lidarr.lidarr for in-cluster)";
              type = types.str;
              default = "lidarr.lidarr";
            };

            port = mkOption {
              description = mdDoc "Lidarr API port";
              type = types.int;
              default = 8686;
            };

            downloadDir = mkOption {
              description = mdDoc "Path inside Lidarr container where it sees Slskd downloads (must match Lidarr root folder)";
              type = types.str;
              default = "/downloads";
            };

            apiKey = mkOption {
              description = mdDoc "Lidarr API key (from Settings > General > Security in Lidarr); stored in a secret";
              type = types.str;
              default = "";
            };
          };

          slskd = {
            host = mkOption {
              description = mdDoc "Slskd service host (e.g. slskd.slskd for in-cluster)";
              type = types.str;
              default = "slskd.slskd";
            };

            port = mkOption {
              description = mdDoc "Slskd API port";
              type = types.int;
              default = 5030;
            };

            apiKey = mkOption {
              description = mdDoc "Slskd API key (same as slskd app); set from secrets.slskd.apiKey; stored in a secret";
              type = types.str;
              default = "";
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
              replicas = 1;
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
                  securityContext.fsGroup = cfg.pgid;
                  serviceAccountName = "default";
                  initContainers = [
                    {
                      name = "write-config";
                      image = "busybox:latest";
                      imagePullPolicy = "IfNotPresent";
                      command = [
                        "sh"
                        "-c"
                        writeConfigScript
                      ];
                      env = [
                        {
                          name = "LIDARR_HOST_URL";
                          value = "http://${cfg.lidarr.host}:${toString cfg.lidarr.port}";
                        }
                        {
                          name = "LIDARR_DOWNLOAD_DIR";
                          value = cfg.lidarr.downloadDir;
                        }
                        {
                          name = "SLSKD_HOST_URL";
                          value = "http://${cfg.slskd.host}:${toString cfg.slskd.port}";
                        }
                        {
                          name = "PUID";
                          value = toString cfg.puid;
                        }
                        {
                          name = "PGID";
                          value = toString cfg.pgid;
                        }
                      ]
                      ++ (lib.optionals (cfg.lidarr.apiKey != "") [
                        {
                          name = "LIDARR_API_KEY";
                          valueFrom.secretKeyRef = {
                            name = lidarr-api-secret;
                            key = "api_key";
                          };
                        }
                      ])
                      ++ (lib.optionals (cfg.slskd.apiKey != "") [
                        {
                          name = "SLSKD_API_KEY";
                          valueFrom.secretKeyRef = {
                            name = slskd-api-secret;
                            key = "api_key";
                          };
                        }
                      ])
                      ++ (lib.optionals (cfg.lidarr.apiKey == "") [
                        {
                          name = "LIDARR_API_KEY";
                          value = "";
                        }
                      ])
                      ++ (lib.optionals (cfg.slskd.apiKey == "") [
                        {
                          name = "SLSKD_API_KEY";
                          value = "";
                        }
                      ]);
                      volumeMounts = [
                        {
                          mountPath = "/data";
                          name = "config";
                        }
                      ];
                    }
                  ];
                  containers = [
                    {
                      inherit name;
                      image = cfg.image;
                      imagePullPolicy = "IfNotPresent";
                      command = [
                        "sh"
                        "-c"
                        "while true; do python soularr.py; sleep ${toString cfg.scriptInterval}; done"
                      ];
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
                        {
                          name = "SCRIPT_INTERVAL";
                          value = toString cfg.scriptInterval;
                        }
                      ];
                      workingDir = "/app";
                      securityContext.runAsUser = cfg.puid;
                      securityContext.runAsGroup = cfg.pgid;
                      volumeMounts = [
                        {
                          mountPath = "/data";
                          name = "config";
                        }
                        {
                          mountPath = "/downloads";
                          name = "downloads";
                        }
                      ];
                    }
                  ];
                  volumes = [
                    cfg.volumes.config.volume
                    {
                      name = "downloads";
                      persistentVolumeClaim.claimName = "${name}-${name}-downloads";
                    }
                  ];
                };
              };
            };
          };

          persistentVolumes = lib.optionalAttrs cfg.nfs.enable {
            "${name}-${name}-downloads-nfs" = {
              apiVersion = "v1";
              metadata.name = "${name}-${name}-downloads-nfs";
              spec = {
                accessModes = [ "ReadWriteMany" ];
                capacity.storage = "1Ti";
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
                  resources.requests.storage = "10Gi";
                };
          };
        };
      };
}
