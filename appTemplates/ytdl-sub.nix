{ config, lib, ... }:
with lib;
(
  let
    name = "ytdl-sub";
    labels."app.kubernetes.io/name" = name;
    configMapName = "${name}-config";
    downloadsPvc = "${name}-${name}-downloads";
    downloadsPv = "${name}-${name}-downloads-nfs";
  in
  {
    inherit name;
    uses-nfs = true;

    extraOptions = {
      image = mkOption {
        description = mdDoc "The ytdl-sub docker image (the headless one, not -gui)";
        type = types.str;
        default = "ghcr.io/jmbannon/ytdl-sub:2026.10.27";
      };

      schedule = mkOption {
        description = mdDoc "Cron schedule for the subscription run";
        type = types.str;
        default = "0 */6 * * *";
      };

      presets = mkOption {
        description = mdDoc ''
          The prebuilt presets every subscription uses, joined with `|`.
          See https://ytdl-sub.readthedocs.io/en/latest/prebuilt_presets/
        '';
        type = types.listOf types.str;
        default = [
          "Plex TV Show by Date"
          "Max 1080p"
          "Only Recent"
        ];
      };

      onlyRecentDateRange = mkOption {
        description = mdDoc ''
          How far back "Only Recent" downloads, and the age after which it
          deletes a video (by upload date, not download date).
        '';
        type = types.str;
        default = "1month";
      };

      subscriptions = mkOption {
        description = mdDoc "Show name -> channel/playlist URL. Each becomes one Plex show folder.";
        type = types.attrsOf types.str;
        default = { };
      };

      outputSubPath = mkOption {
        description = mdDoc "Directory under the NFS mount that the shows are written to";
        type = types.str;
        default = "ytdl-sub";
      };
    };

    extraResources = cfg: {
      configMaps.${configMapName}.data = {
        # The image defaults lock_directory to /config, which is this
        # read-only ConfigMap.
        "config.yaml" = builtins.toJSON {
          configuration = {
            working_directory = "/tmp/ytdl-sub-working";
            lock_directory = "/tmp";
          };
        };
        "subscriptions.yaml" = builtins.toJSON {
          __preset__.overrides = {
            tv_show_directory = "/downloads/${cfg.outputSubPath}";
            only_recent_date_range = cfg.onlyRecentDateRange;
          };
          ${concatStringsSep " | " cfg.presets} = cfg.subscriptions;
        };
      };

      cronJobs.${name} = {
        metadata.labels = labels;
        spec = {
          inherit (cfg) schedule;
          # The first run over every channel can outlast the interval.
          concurrencyPolicy = "Forbid";
          successfulJobsHistoryLimit = 3;
          failedJobsHistoryLimit = 3;
          jobTemplate.spec = {
            backoffLimit = 0;
            template = {
              metadata.labels = labels;
              spec = {
                restartPolicy = "Never";
                containers = [
                  {
                    inherit name;
                    inherit (cfg) image;
                    imagePullPolicy = "IfNotPresent";
                    # Replaces the s6 /init entrypoint: one run, then exit.
                    command = [
                      "ytdl-sub"
                      "--config"
                      "/config/config.yaml"
                      "sub"
                      "/config/subscriptions.yaml"
                    ];
                    resources = {
                      requests = {
                        memory = "256Mi";
                        cpu = "100m";
                      };
                      limits = {
                        memory = "2Gi";
                        cpu = "2000m";
                      };
                    };
                    volumeMounts = [
                      {
                        name = "config";
                        mountPath = "/config";
                        readOnly = true;
                      }
                      {
                        name = "downloads";
                        mountPath = "/downloads";
                      }
                      {
                        name = "tmp";
                        mountPath = "/tmp";
                      }
                    ];
                  }
                ];
                volumes = [
                  {
                    name = "config";
                    configMap.name = configMapName;
                  }
                  {
                    name = "downloads";
                    persistentVolumeClaim.claimName = downloadsPvc;
                  }
                  {
                    name = "tmp";
                    emptyDir = { };
                  }
                ];
              };
            };
          };
        };
      };

      # Download archives live next to the videos (.ytdl-sub-*.json), so the
      # NAS holds all of ytdl-sub's state; there's no volume to pin.
      persistentVolumeClaims.${downloadsPvc}.spec = {
        accessModes = [ "ReadWriteMany" ];
        resources.requests.storage = "1Gi";
        storageClassName = "";
        volumeName = downloadsPv;
      };

      persistentVolumes.${downloadsPv} = {
        apiVersion = "v1";
        kind = "PersistentVolume";
        metadata.name = downloadsPv;
        spec = {
          capacity.storage = "2Ti";
          accessModes = [ "ReadWriteMany" ];
          # hard: a NAS stall must block, not make files look missing.
          mountOptions = [
            "nolock"
            "nfsvers=3"
            "noexec"
            "hard"
            "timeo=600"
          ];
          nfs = {
            server = cfg.nfs.server;
            path = cfg.nfs.path;
          };
          persistentVolumeReclaimPolicy = "Retain";
        };
      };
    };
  }
)
