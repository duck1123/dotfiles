{ ... }:
{
  flake.nixidyApps.immich =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      password-secret = "immich-database-password";
      redis-secret = "immich-redis-password";
      cfg = config.services.immich;
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
        name = "immich";
        uses-ingress = true;
        uses-database = true;

        sopsSecrets = cfg: {
          ${password-secret} = {
            inherit (cfg.database) password username;
          };
          ${redis-secret} = {
            password = cfg.redis.password;
          };
        };

        extraAppConfig = cfg: {
          annotations."argocd.argoproj.io/sync-wave" = "2";
        };

        # https://github.com/immich-app/immich-charts
        chart = lib.helm.downloadHelmChart {
          repo = "oci://ghcr.io/immich-app/immich-charts";
          chart = "immich";
          version = "0.12.0";
          chartHash = "sha256-Lfx0JwdG65oTeql/qEBF6OOgqYw9AMU+uEdI0Yi5fuQ=";
        };

        extraOptions = {
          image.tag = mkOption {
            description = mdDoc "The docker image tag";
            type = types.str;
            default = "release";
          };

          adminApiKey = mkOption {
            description = mdDoc ''
              Immich admin API key (Immich UI -> Account Settings -> API Keys).
              Stored in secrets.enc.yaml as `immich.adminApiKey` and wired in via
              `env/dev/immich.nix`. Not used by the immich container itself --
              only powers the auto-added homepage dashboard widget below (see
              `homepage.extraSettings.widget`), which calls Immich's API to show
              photo/storage stats.
            '';
            type = types.str;
            default = "";
          };

          # Auto-add an Immich widget to this app's homepage dashboard tile once
          # an admin API key is configured (see `adminApiKey` above) -- no manual
          # `homepage.extraSettings` wiring needed per-environment. The actual key
          # value is injected via homepage's `{{HOMEPAGE_VAR_IMMICH_API_KEY}}`
          # placeholder (see applications/homepage.nix's `widgetSecrets`); set
          # `services.homepage.widgetSecrets.IMMICH_API_KEY` from
          # `config.services.immich.adminApiKey` in env/dev/homepage.nix.
          # `version = 2` selects homepage's newer Immich API paths
          # (/api/server/*, required for Immich >= v1.118 -- this repo tracks
          # the "release" image tag, currently v2.x); the API key must carry
          # the `server.statistics` permission or homepage's requests 403.
          homepage.extraSettings = mkOption {
            default = lib.optionalAttrs (cfg.adminApiKey != "") {
              widget = {
                type = "immich";
                url = "http://${name}-server.${cfg.namespace}:2283";
                key = "{{HOMEPAGE_VAR_IMMICH_API_KEY}}";
                version = 2;
              };
            };
          };

          replicas = mkOption {
            description = mdDoc "Number of immich-server replicas";
            type = types.int;
            default = 1;
          };

          redis = {
            host = mkOption {
              description = mdDoc "The Redis host";
              type = types.str;
              default = "redis";
            };

            port = mkOption {
              description = mdDoc "The Redis port";
              type = types.int;
              default = 6379;
            };

            password = mkOption {
              description = mdDoc "The Redis password";
              type = types.str;
              default = "CHANGEME";
            };

            dbIndex = mkOption {
              description = mdDoc "The Redis database index";
              type = types.int;
              default = 0;
            };
          };

          nfs = {
            enable = mkOption {
              description = mdDoc "Enable NFS for library volume";
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
              default = "/mnt/photos";
            };
          };

          # Set per-environment (e.g. env/dev/immich.nix), same reasoning as
          # cfg.volumeOverrides.<key>.volumeHandle elsewhere -- see
          # docs/pinned-volumes.md. Only meaningful when cfg.nfs.enable is
          # false; the library volume conditionally swaps between a pinned
          # Longhorn volume and an NFS-backed one under the same PVC name
          # (see extraResources below), which doesn't fit the generic
          # volumes/volumeOverrides model, so there is no automatic
          # dynamic-PVC fallback for a null value here.
          libraryVolumeHandle = mkOption {
            description = mdDoc "Longhorn volumeHandle to pin the library volume to when cfg.nfs.enable is false.";
            type = types.nullOr types.str;
            default = null;
          };

          externalLibrary = {
            enable = mkOption {
              description = mdDoc "Mount an NFS share as an external library (read-only) at /mnt/external-library";
              type = types.bool;
              default = false;
            };

            server = mkOption {
              description = mdDoc "NFS server hostname/IP for external library";
              type = types.str;
              default = "nasnix";
            };

            path = mkOption {
              description = mdDoc "NFS server path for external library";
              type = types.str;
              default = "/mnt/photos";
            };
          };
        };

        defaultValues = cfg: {
          # Disable built-in Redis (Valkey)
          # Note: postgresql subchart was removed in 0.10.0, must be deployed separately
          valkey.enabled = false;

          # Pin every Immich component (server, machine-learning) to the same node.
          controllers.main.pod = lib.optionalAttrs (cfg.hostAffinity != null) {
            nodeSelector."kubernetes.io/hostname" = cfg.hostAffinity;
          };

          # Environment variables for all Immich components
          controllers.main.containers.main.env = {
            DB_HOSTNAME = cfg.database.host;
            DB_PORT = "${toString cfg.database.port}";
            DB_USERNAME.valueFrom.secretKeyRef = {
              name = password-secret;
              key = "username";
            };
            DB_PASSWORD.valueFrom.secretKeyRef = {
              name = password-secret;
              key = "password";
            };
            DB_DATABASE_NAME = cfg.database.name;

            # Redis configuration - override default valkey hostname
            REDIS_HOSTNAME = cfg.redis.host;
            REDIS_PORT = "${toString cfg.redis.port}";
            REDIS_PASSWORD.valueFrom.secretKeyRef = {
              name = redis-secret;
              key = "password";
            };
            REDIS_DBINDEX = "${toString cfg.redis.dbIndex}";
          };

          immich = {
            image.tag = cfg.image.tag;

            # Persistence configuration
            persistence = {
              library.existingClaim = "${name}-${name}-library";
              upload = {
                enabled = true;
                storageClass = cfg.storageClassName;
                accessMode = "ReadWriteOnce";
                size = "10Gi";
              };
              thumbs = {
                enabled = true;
                storageClass = cfg.storageClassName;
                accessMode = "ReadWriteOnce";
                size = "50Gi";
              };
              ml = {
                enabled = true;
                storageClass = cfg.storageClassName;
                accessMode = "ReadWriteOnce";
                size = "10Gi";
              };
              config = {
                enabled = true;
                storageClass = cfg.storageClassName;
                accessMode = "ReadWriteOnce";
                size = "1Gi";
              };
            };
          };

          server = {
            # Single-replica deployment backed by a ReadWriteOnce volume: RollingUpdate
            # can deadlock (new pod can't attach the volume until the old one releases it).
            controllers.main.strategy = "Recreate";
            controllers.main.replicas = cfg.replicas;
          }
          // lib.optionalAttrs cfg.externalLibrary.enable {
            persistence.external-library = {
              existingClaim = "${name}-${name}-external-library";
              globalMounts = [
                {
                  path = "/mnt/external-library";
                  readOnly = true;
                }
              ];
            };
          };

          ingress.main.enabled = false;
        };

        extraResources =
          cfg:
          let
            # cfg.libraryVolumeHandle (env-settable, see docs/pinned-volumes.md) for
            # the 100Gi photo/video library -- pins disable/re-enable cycles to the
            # same data instead of a fresh empty PVC.
            pinnedLibrary = self.lib.mkPinnedVolume {
              pvcName = "${name}-${name}-library";
              volumeHandle = cfg.libraryVolumeHandle;
              size = "100Gi";
            };
          in
          {
            ingresses.${name} = {
              metadata.annotations."cert-manager.io/cluster-issuer" = cfg.ingress.clusterIssuer;

              spec = with cfg.ingress; {
                inherit ingressClassName;

                rules = [
                  {
                    host = domain;

                    http.paths = [
                      {
                        backend.service = {
                          name = "${name}-server";
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

            # Create NFS PersistentVolumes when NFS options are enabled
            persistentVolumes =
              (lib.optionalAttrs cfg.nfs.enable {
                "${name}-${name}-library-nfs" = {
                  apiVersion = "v1";
                  kind = "PersistentVolume";
                  metadata.name = "${name}-${name}-library-nfs";
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
              })
              // (lib.optionalAttrs (!cfg.nfs.enable) pinnedLibrary.persistentVolumes)
              // (lib.optionalAttrs cfg.externalLibrary.enable {
                "${name}-${name}-external-library-nfs" = {
                  apiVersion = "v1";
                  kind = "PersistentVolume";
                  metadata.name = "${name}-${name}-external-library-nfs";
                  spec = {
                    accessModes = [ "ReadOnlyMany" ];
                    capacity.storage = "1Ti";
                    mountOptions = [
                      "nolock"
                      "noexec"
                      "soft"
                      "timeo=30"
                    ];
                    nfs = {
                      server = cfg.externalLibrary.server;
                      path = cfg.externalLibrary.path;
                    };
                    persistentVolumeReclaimPolicy = "Retain";
                  };
                };
              });

            # NFS-backed PVC when NFS is enabled; otherwise pin to the existing
            # dynamically-provisioned Longhorn volume (see pinnedLibrary above) so
            # disable/re-enable cycles rebind to the same photo library.
            persistentVolumeClaims =
              (
                if cfg.nfs.enable then
                  {
                    "${name}-${name}-library".spec = {
                      accessModes = [ "ReadWriteMany" ];
                      resources.requests.storage = "1Gi";
                      storageClassName = "";
                      volumeName = "${name}-${name}-library-nfs";
                    };
                  }
                else
                  pinnedLibrary.persistentVolumeClaims
              )
              // (lib.optionalAttrs cfg.externalLibrary.enable {
                "${name}-${name}-external-library".spec = {
                  accessModes = [ "ReadOnlyMany" ];
                  resources.requests.storage = "1Gi";
                  storageClassName = "";
                  volumeName = "${name}-${name}-external-library-nfs";
                };
              });

            # Job to enable vector extension in PostgreSQL database
            # Uses ArgoCD sync hook to run after secrets are created but before Immich is deployed
            jobs = {
              "${name}-enable-vector-extension" = {
                metadata.annotations = {
                  "argocd.argoproj.io/hook" = "Sync";
                  "argocd.argoproj.io/hook-delete-policy" = "BeforeHookCreation,HookSucceeded";
                  "argocd.argoproj.io/sync-wave" = "1";
                };
                spec = {
                  backoffLimit = 3;
                  template.spec = {
                    restartPolicy = "OnFailure";
                    containers = [
                      {
                        name = "enable-vector-extension";
                        image = "docker.io/postgres:17.11";
                        imagePullPolicy = "IfNotPresent";
                        command = [ "psql" ];
                        args = [
                          "-h"
                          cfg.database.host
                          "-p"
                          "${toString cfg.database.port}"
                          "-U"
                          cfg.database.username
                          "-d"
                          cfg.database.name
                          "-c"
                          "CREATE EXTENSION IF NOT EXISTS vector;"
                        ];
                        env = [
                          {
                            name = "PGPASSWORD";
                            valueFrom.secretKeyRef = {
                              name = password-secret;
                              key = "password";
                            };
                          }
                        ];
                      }
                    ];
                  };
                };
              };
            };
          };
      };
}
