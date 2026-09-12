{ ... }:
{
  flake.nixidyApps.garage =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      name = "garage";
      secrets-name = "garage-secrets";
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
        inherit name;
        uses-ingress = true;

        # Everything here is required for garage to start at all (rpc_secret
        # and admin_token must be non-empty or the process refuses to boot),
        # so unlike rustfs's optional credentials this secret is unconditional.
        # Values come from the top-level secrets.enc.yaml (`secrets.garage.*`,
        # set via `nur secrets edit`) — see env/dev/garage.nix.
        sopsSecrets = cfg: {
          ${secrets-name} = {
            GARAGE_RPC_SECRET = cfg.rpcSecret;
            GARAGE_ADMIN_TOKEN = cfg.adminToken;
            GARAGE_DEFAULT_ACCESS_KEY = cfg.accessKey;
            GARAGE_DEFAULT_SECRET_KEY = cfg.secretKey;
          };
        };

        # No published Helm chart repo exists upstream (only a chart living
        # inside the garage git repo, meant to be cloned+installed locally) —
        # hand-rolled resources give the same control with no extra fetch.

        # Only `meta` goes through here -- `data` conditionally swaps between a
        # pinned Longhorn volume and an NFS-backed one under the same PVC name
        # depending on cfg.nfs.enable (see extraResources below), which doesn't
        # fit this shape+override model, so it keeps its own hand-rolled
        # mkPinnedVolume call. Shape only -- no volumeHandle here, that's
        # environment-specific (see env/dev/garage.nix and docs/pinned-volumes.md).
        volumes = cfg: {
          meta = {
            pvcName = "${name}-meta";
            size = cfg.metaPersistenceSize;
          };
        };

        extraOptions = {
          image = mkOption {
            description = mdDoc "The garage container image";
            type = types.str;
            default = "dxflrs/garage:v2.3.0";
          };

          # 64 hex chars, e.g. `openssl rand -hex 32`. Only matters for
          # inter-node auth, but garage still refuses to start without one
          # even in --single-node mode.
          rpcSecret = mkOption {
            description = mdDoc "RPC secret shared between cluster nodes (openssl rand -hex 32)";
            type = types.str;
            default = "";
          };

          # e.g. `openssl rand -base64 32`. Guards the admin API (bucket/key
          # management, cluster layout) on port 3903.
          adminToken = mkOption {
            description = mdDoc "Bearer token for the admin API (openssl rand -base64 32)";
            type = types.str;
            default = "";
          };

          accessKey = mkOption {
            description = mdDoc "S3 access key ID for the default bucket/key created on boot";
            type = types.str;
            default = "";
          };

          secretKey = mkOption {
            description = mdDoc "S3 secret access key for the default bucket/key created on boot";
            type = types.str;
            default = "";
          };

          defaultBucket = mkOption {
            description = mdDoc "Bucket name created and granted to accessKey on first boot";
            type = types.str;
            default = "default";
          };

          metaPersistenceSize = mkOption {
            description = mdDoc "Size of the metadata volume (lmdb — keep this off NFS)";
            type = types.str;
            default = "2Gi";
          };

          dataPersistenceSize = mkOption {
            description = mdDoc "Size of the data-blocks volume";
            type = types.str;
            default = "50Gi";
          };

          # Set per-environment (e.g. env/dev/garage.nix), same reasoning as
          # cfg.volumeOverrides.<key>.volumeHandle elsewhere -- see
          # docs/pinned-volumes.md. Only meaningful when cfg.nfs.enable is
          # false; unlike the generic volumes/volumeOverrides mechanism, there
          # is no automatic dynamic-PVC fallback for a null value here, since
          # this volume's shape doesn't fit that model (see the extraResources
          # comment above).
          dataVolumeHandle = mkOption {
            description = mdDoc "Longhorn volumeHandle to pin the data volume to when cfg.nfs.enable is false.";
            type = types.nullOr types.str;
            default = null;
          };

          # Chart default is n/a here (no chart); the container has no
          # documented non-root user, so this only takes effect when nfs.enable
          # forces a podSecurityContext to match the NFS export's expected uid.
          uid = mkOption {
            description = mdDoc ''
              uid/gid to run the garage pod as, applied only when nfs.enable is
              set. Needs to match a uid the NFS server actually grants write
              access to, since NFS permission checks happen server-side
              regardless of "no mapping" squash settings.
            '';
            type = types.int;
            default = 1000;
          };

          nfs = {
            enable = mkOption {
              description = mdDoc "Back the data-blocks volume with NFS instead of a StorageClass (metadata always stays on a StorageClass — lmdb doesn't tolerate NFS)";
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
        };

        extraResources =
          cfg:
          let
            pinnedData = self.lib.mkPinnedVolume {
              pvcName = "${name}-data";
              volumeHandle = cfg.dataVolumeHandle;
              size = cfg.dataPersistenceSize;
            };
            nfsDataPV = lib.optionalAttrs cfg.nfs.enable {
              "${name}-data-nfs".spec = {
                capacity.storage = "1Ti";
                accessModes = [ "ReadWriteOnce" ];
                storageClassName = "";
                claimRef = {
                  name = "${name}-data";
                  namespace = cfg.namespace;
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
            };
          in
          {
            configMaps."${name}-config".data."garage.toml" = ''
              metadata_dir = "/data/meta"
              data_dir = "/data/data"
              db_engine = "lmdb"

              replication_factor = 1

              rpc_bind_addr = "[::]:3901"
              rpc_public_addr = "127.0.0.1:3901"

              [s3_api]
              # "us-east-1" despite this being self-hosted, not AWS: attic-server's
              # get_nar handler hardcodes prefer_stream=false for single-chunk NARs
              # (github.com/zhaofengli/attic server/src/api/binary_cache.rs), which
              # routes those downloads through presigned S3 URLs. Its presigning
              # codepath signs with us-east-1 unconditionally, ignoring both
              # [storage].region in server.toml and AWS_REGION/AWS_DEFAULT_REGION --
              # so Garage must accept that literal region string regardless of what
              # attic's own config claims. See applications/attic.nix and
              # applications/xyops.nix, which must both match this value.
              s3_region = "us-east-1"
              api_bind_addr = "[::]:3900"

              [admin]
              api_bind_addr = "[::]:3903"
            '';

            deployments.${name} = {
              metadata.labels = {
                "app.kubernetes.io/instance" = name;
                "app.kubernetes.io/name" = name;
              };

              spec = {
                replicas = 1;
                # Single ReadWriteOnce-backed replica — a RollingUpdate would
                # try to start the new pod before the old one releases the
                # PVCs and deadlock.
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
                    securityContext = lib.optionalAttrs cfg.nfs.enable {
                      fsGroup = cfg.uid;
                      runAsUser = cfg.uid;
                      runAsGroup = cfg.uid;
                    };

                    containers = [
                      {
                        inherit name;
                        image = cfg.image;
                        imagePullPolicy = "IfNotPresent";
                        command = [
                          "/garage"
                          "server"
                          "--single-node"
                          "--default-bucket"
                        ];

                        env = [
                          {
                            name = "GARAGE_RPC_SECRET";
                            valueFrom.secretKeyRef = {
                              name = secrets-name;
                              key = "GARAGE_RPC_SECRET";
                            };
                          }
                          {
                            name = "GARAGE_ADMIN_TOKEN";
                            valueFrom.secretKeyRef = {
                              name = secrets-name;
                              key = "GARAGE_ADMIN_TOKEN";
                            };
                          }
                          {
                            name = "GARAGE_DEFAULT_ACCESS_KEY";
                            valueFrom.secretKeyRef = {
                              name = secrets-name;
                              key = "GARAGE_DEFAULT_ACCESS_KEY";
                            };
                          }
                          {
                            name = "GARAGE_DEFAULT_SECRET_KEY";
                            valueFrom.secretKeyRef = {
                              name = secrets-name;
                              key = "GARAGE_DEFAULT_SECRET_KEY";
                            };
                          }
                          {
                            name = "GARAGE_DEFAULT_BUCKET";
                            value = cfg.defaultBucket;
                          }
                        ];

                        ports = [
                          {
                            containerPort = 3900;
                            name = "s3";
                            protocol = "TCP";
                          }
                          {
                            containerPort = 3901;
                            name = "rpc";
                            protocol = "TCP";
                          }
                          {
                            containerPort = 3903;
                            name = "admin";
                            protocol = "TCP";
                          }
                        ];

                        readinessProbe = {
                          httpGet = {
                            path = "/health";
                            port = "admin";
                          };
                          initialDelaySeconds = 10;
                          periodSeconds = 10;
                          timeoutSeconds = 5;
                          failureThreshold = 3;
                        };

                        livenessProbe = {
                          httpGet = {
                            path = "/health";
                            port = "admin";
                          };
                          initialDelaySeconds = 30;
                          periodSeconds = 30;
                          timeoutSeconds = 5;
                          failureThreshold = 3;
                        };

                        volumeMounts = [
                          {
                            mountPath = "/etc/garage.toml";
                            name = "config";
                            subPath = "garage.toml";
                          }
                          {
                            mountPath = "/data/meta";
                            name = "meta";
                          }
                          {
                            mountPath = "/data/data";
                            name = "data";
                          }
                        ];
                      }
                    ];

                    volumes = [
                      {
                        name = "config";
                        configMap.name = "${name}-config";
                      }
                      cfg.volumes.meta.volume
                      {
                        name = "data";
                        persistentVolumeClaim.claimName = "${name}-data";
                      }
                    ];
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
                        path = "/";
                        pathType = "Prefix";
                        backend.service = {
                          inherit name;
                          port.name = "s3";
                        };
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

            services.${name}.spec = {
              type = "ClusterIP";
              selector = {
                "app.kubernetes.io/instance" = name;
                "app.kubernetes.io/name" = name;
              };
              ports = [
                {
                  name = "s3";
                  port = 3900;
                  protocol = "TCP";
                  targetPort = "s3";
                }
                {
                  name = "rpc";
                  port = 3901;
                  protocol = "TCP";
                  targetPort = "rpc";
                }
                {
                  name = "admin";
                  port = 3903;
                  protocol = "TCP";
                  targetPort = "admin";
                }
              ];
            };

            persistentVolumeClaims = {
              "${name}-data" =
                if cfg.nfs.enable then
                  {
                    spec = {
                      accessModes = [ "ReadWriteOnce" ];
                      resources.requests.storage = cfg.dataPersistenceSize;
                      storageClassName = "";
                      volumeName = "${name}-data-nfs";
                    };
                  }
                else
                  pinnedData.persistentVolumeClaims."${name}-data";
            };

            persistentVolumes = nfsDataPV // lib.optionalAttrs (!cfg.nfs.enable) pinnedData.persistentVolumes;
          };
      };
}
