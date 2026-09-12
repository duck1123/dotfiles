{ ... }:
{
  flake.nixidyApps.rustfs =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      credentials-secret = "rustfs-credentials";
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
      {
        name = "rustfs";

        sopsSecrets =
          cfg:
          optionalAttrs (cfg.accessKey != "" && cfg.secretKey != "") {
            ${credentials-secret} = {
              RUSTFS_ACCESS_KEY = cfg.accessKey;
              RUSTFS_SECRET_KEY = cfg.secretKey;
            };
          };

        # https://artifacthub.io/packages/helm/rustfs/rustfs
        chart = lib.helm.downloadHelmChart {
          repo = "https://charts.rustfs.com/";
          chart = "rustfs";
          version = "0.0.90";
          chartHash = "sha256-QoBu6mNbuJeF8DZLTQfG+QhZP/mU2ZD/uq6TZbPbqpU=";
        };

        uses-ingress = true;

        extraOptions = {
          ingress.api-domain = mkOption {
            description = mdDoc "The ingress domain for the S3 API";
            type = types.str;
            default = "api-rustfs.local";
          };

          accessKey = mkOption {
            description = mdDoc "RustFS access key (S3 Access Key ID)";
            type = types.str;
            default = "";
          };

          secretKey = mkOption {
            description = mdDoc "RustFS secret key (S3 Secret Access Key)";
            type = types.str;
            default = "";
          };

          mode = mkOption {
            description = mdDoc "Deployment mode: standalone (single pod) or distributed";
            type = types.enum [
              "standalone"
              "distributed"
            ];
            default = "standalone";
          };

          nfs = {
            enable = mkOption {
              description = mdDoc "Use NFS for data volume instead of a StorageClass";
              type = types.bool;
              default = false;
            };

            server = mkOption {
              description = mdDoc "NFS server hostname/IP";
              type = types.str;
              default = "";
            };

            path = mkOption {
              description = mdDoc "NFS export path";
              type = types.str;
              default = "";
            };
          };

          uid = mkOption {
            description = mdDoc ''
              uid/gid to run the rustfs pod as (podSecurityContext.runAsUser/runAsGroup/fsGroup).
              Chart default is 10001. When backed by NFS, this needs to match a uid the NFS
              server actually grants write access to (e.g. the NAS account's uid), since NFS
              permission checks happen server-side regardless of "no mapping" squash settings.
            '';
            type = types.int;
            default = 10001;
          };

          logRetention = {
            keepFiles = mkOption {
              description = mdDoc ''
                Number of rotated log files to keep in /logs before the oldest are deleted
                (RUSTFS_OBS_LOG_KEEP_FILES). Chart/binary default is 30, but its log cleaner
                has been observed to stop enforcing this under a large backlog, filling the
                logs PVC and crash-looping the pod - kept lower here as a safety margin.
              '';
              type = types.int;
              default = 14;
            };

            rotationTime = mkOption {
              description = mdDoc "Log rotation interval (RUSTFS_OBS_LOG_ROTATION_TIME).";
              type = types.enum [
                "minute"
                "hour"
                "day"
              ];
              default = "hour";
            };

            rotationSizeMb = mkOption {
              description = mdDoc "Rotate the current log file once it exceeds this size in MB (RUSTFS_OBS_LOG_ROTATION_SIZE_MB).";
              type = types.int;
              default = 100;
            };
          };
        };

        extraResources =
          cfg:
          (lib.optionalAttrs cfg.nfs.enable {
            persistentVolumes."rustfs-data-nfs".spec = {
              capacity.storage = "1Ti";
              accessModes = [ "ReadWriteOnce" ];
              storageClassName = "";
              claimRef = {
                name = "rustfs-data";
                namespace = "rustfs";
              };
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
          })
          // {
            # The chart's own Ingress template hardcodes its backend to the
            # "console" port for every host in ingress.hosts, so there's no
            # way to expose the S3 API (the "endpoint" port) through it. Any
            # S3 client outside the cluster (e.g. atticd's presigned URLs)
            # needs this reachable from somewhere other than the in-cluster
            # rustfs-svc ClusterIP, so wire it up by hand.
            ingresses.rustfs-api = {
              metadata.annotations."cert-manager.io/cluster-issuer" = cfg.ingress.clusterIssuer;
              spec = {
                ingressClassName = cfg.ingress.ingressClassName;
                rules = [
                  {
                    host = cfg.ingress.api-domain;
                    http.paths = [
                      {
                        path = "/";
                        pathType = "Prefix";
                        backend.service = {
                          name = "rustfs-svc";
                          port.name = "endpoint";
                        };
                      }
                    ];
                  }
                ];
                tls = [
                  {
                    hosts = [ cfg.ingress.api-domain ];
                    secretName = "rustfs-api-tls";
                  }
                ];
              };
            };
          };

        defaultValues =
          cfg:
          {
            replicaCount = if cfg.mode == "standalone" then 1 else 4;
            mode = {
              standalone.enabled = cfg.mode == "standalone";
              distributed.enabled = cfg.mode == "distributed";
            };

            secret =
              if cfg.accessKey != "" && cfg.secretKey != "" then
                { existingSecret = credentials-secret; }
              else
                {
                  rustfs = {
                    access_key = "rustfsadmin";
                    secret_key = "rustfsadmin";
                  };
                };

            storageclass.name = if cfg.nfs.enable then "" else cfg.storageClassName;

            config.rustfs.log_rotation = {
              keep_files = cfg.logRetention.keepFiles;
              time = cfg.logRetention.rotationTime;
              size = cfg.logRetention.rotationSizeMb;
            };

            podSecurityContext = {
              fsGroup = cfg.uid;
              runAsUser = cfg.uid;
              runAsGroup = cfg.uid;
            };

            ingress = with cfg.ingress; {
              enabled = true;
              className = ingressClassName;
              customAnnotations = {
                "cert-manager.io/cluster-issuer" = clusterIssuer;
              };
              hosts = [
                {
                  host = domain;
                  paths = [
                    {
                      path = "/";
                      pathType = "Prefix";
                    }
                  ];
                }
              ];
              tls = {
                enabled = tls.enable;
                certManager.enabled = false;
                # cert-manager already manages this Secret via the Ingress's
                # cert-manager.io/cluster-issuer annotation. Point the chart at
                # it as an "existing secret" so it doesn't also render its own
                # placeholder Secret (templates/secret-tls.yaml) with dummy
                # tls.crt/tls.key values that ArgoCD would reapply over the
                # real cert-manager-issued data on every sync.
                existingSecret = {
                  enabled = true;
                  name = "rustfs-tls";
                };
              };
            };
          }
          // optionalAttrs (cfg.hostAffinity != null) {
            nodeSelector."kubernetes.io/hostname" = cfg.hostAffinity;
          };
      };
}
