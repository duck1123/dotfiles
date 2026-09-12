{ ... }:
{
  flake.nixidyApps.xyops =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      name = "xyops";
      db-secret = "${name}-db";
      s3-secret = "${name}-s3";
      app-secret = "${name}-app";
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
        uses-database = true;

        # `pvcName = "${name}-data"` (not the usual "${name}-${name}-<key>"
        # convention). Shape only -- no volumeHandle here, that's
        # environment-specific (see env/dev/xyops.nix and docs/pinned-volumes.md).
        volumes = cfg: {
          data = {
            pvcName = "${name}-data";
            size = cfg.persistenceSize;
          };
        };

        # xyOps' Hybrid storage engine only supports one JSON doc-store
        # (Postgres or Redis) paired with one binary store, not all three at
        # once -- Postgres (via uses-database) is the docEngine here, S3
        # (garage) is the binaryEngine. All storage config below is passed
        # as XYOPS_Storage__* env vars (pixlcore/xyops docs: "Environment
        # variables work well for scalar values"), which keeps secrets out
        # of a mounted config.json.
        sopsSecrets =
          cfg:
          optionalAttrs (cfg.database.password != "") {
            ${db-secret}.PASSWORD = cfg.database.password;
          }
          // optionalAttrs (cfg.s3.accessKey != "" && cfg.s3.secretKey != "") {
            ${s3-secret} = {
              ACCESS_KEY = cfg.s3.accessKey;
              SECRET_KEY = cfg.s3.secretKey;
            };
          }
          // optionalAttrs (cfg.secretKey != "") {
            ${app-secret}.SECRET_KEY = cfg.secretKey;
          };

        extraOptions = {
          image = mkOption {
            description = mdDoc "The xyOps container image";
            type = types.str;
            default = "ghcr.io/pixlcore/xyops:latest";
          };

          # See docs/config.md#secret_key upstream: signs tokens, and
          # authenticates multi-conductor messages. Empty = xyOps generates
          # and persists one under /opt/xyops/data on first boot, which is
          # fine for a single-conductor trial.
          secretKey = mkOption {
            description = mdDoc "xyOps secret_key (openssl rand -hex 32). Empty = auto-generated and persisted on first boot.";
            type = types.str;
            default = "";
          };

          persistenceSize = mkOption {
            description = mdDoc "Size of the /opt/xyops volume (temp files, plugin bundles, conf overrides -- records/files live in Postgres+S3)";
            type = types.str;
            default = "5Gi";
          };

          s3 = {
            bucket = mkOption {
              description = mdDoc "S3 bucket used for xyOps binary storage (attachments, job logs, uploads)";
              type = types.str;
              default = "default";
            };

            keyPrefix = mkOption {
              description = mdDoc "Key prefix within the bucket, to namespace xyOps objects from other tenants of the same bucket";
              type = types.str;
              default = "xyops/";
            };

            region = mkOption {
              description = mdDoc "S3 region (must match garage's configured s3_region, see applications/garage.nix)";
              type = types.str;
              default = "us-east-1";
            };

            endpoint = mkOption {
              description = mdDoc "S3-compatible endpoint URL";
              type = types.str;
              default = "http://garage.garage:3900";
            };

            accessKey = mkOption {
              description = mdDoc "S3 access key ID";
              type = types.str;
              default = "";
            };

            secretKey = mkOption {
              description = mdDoc "S3 secret access key";
              type = types.str;
              default = "";
            };
          };
        };

        extraResources =
          cfg:
          let
            envVars = [
              {
                name = "TZ";
                value = cfg.tz;
              }
              {
                name = "XYOPS_hostname";
                value = "${name}.${cfg.namespace}";
              }
              {
                name = "XYOPS_masters";
                value = "${name}.${cfg.namespace}";
              }
              {
                name = "XYOPS_base_app_url";
                value = "https://${cfg.ingress.domain}";
              }
              {
                name = "XYOPS_xysat_local";
                value = "true";
              }
              {
                name = "XYOPS_foreground";
                value = "true";
              }
              {
                name = "XYOPS_echo";
                value = "true";
              }

              {
                name = "XYOPS_Storage__engine";
                value = "Hybrid";
              }
              {
                name = "XYOPS_Storage__Hybrid__docEngine";
                value = "Postgres";
              }
              {
                name = "XYOPS_Storage__Hybrid__binaryEngine";
                value = "S3";
              }

              {
                name = "XYOPS_Storage__Postgres__host";
                value = cfg.database.host;
              }
              {
                name = "XYOPS_Storage__Postgres__port";
                value = toString cfg.database.port;
              }
              {
                name = "XYOPS_Storage__Postgres__database";
                value = cfg.database.name;
              }
              {
                name = "XYOPS_Storage__Postgres__user";
                value = cfg.database.username;
              }
              {
                name = "XYOPS_Storage__Postgres__table";
                value = "xyops";
              }

              {
                name = "XYOPS_Storage__AWS__endpoint";
                value = cfg.s3.endpoint;
              }
              {
                name = "XYOPS_Storage__AWS__endpointPrefix";
                value = "false";
              }
              {
                name = "XYOPS_Storage__AWS__forcePathStyle";
                value = "true";
              }
              {
                name = "XYOPS_Storage__AWS__region";
                value = cfg.s3.region;
              }

              {
                name = "XYOPS_Storage__S3__params__Bucket";
                value = cfg.s3.bucket;
              }
              {
                name = "XYOPS_Storage__S3__keyPrefix";
                value = cfg.s3.keyPrefix;
              }
            ]
            ++ optionals (cfg.database.password != "") [
              {
                name = "XYOPS_Storage__Postgres__password";
                valueFrom.secretKeyRef = {
                  name = db-secret;
                  key = "PASSWORD";
                };
              }
            ]
            ++ optionals (cfg.s3.accessKey != "" && cfg.s3.secretKey != "") [
              {
                name = "XYOPS_Storage__AWS__credentials__accessKeyId";
                valueFrom.secretKeyRef = {
                  name = s3-secret;
                  key = "ACCESS_KEY";
                };
              }
              {
                name = "XYOPS_Storage__AWS__credentials__secretAccessKey";
                valueFrom.secretKeyRef = {
                  name = s3-secret;
                  key = "SECRET_KEY";
                };
              }
            ]
            ++ optionals (cfg.secretKey != "") [
              {
                name = "XYOPS_secret_key";
                valueFrom.secretKeyRef = {
                  name = app-secret;
                  key = "SECRET_KEY";
                };
              }
            ];
          in
          {
            deployments.${name} = {
              metadata.labels = {
                "app.kubernetes.io/instance" = name;
                "app.kubernetes.io/name" = name;
              };

              spec = {
                replicas = 1;
                # Single ReadWriteOnce-backed replica -- a RollingUpdate would
                # try to start the new pod before the old one releases the PVC.
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
                    containers = [
                      {
                        inherit name;
                        image = cfg.image;
                        imagePullPolicy = "IfNotPresent";
                        env = envVars;

                        ports = [
                          {
                            containerPort = 5522;
                            name = "http";
                            protocol = "TCP";
                          }
                        ];

                        readinessProbe = {
                          httpGet = {
                            path = "/";
                            port = "http";
                          };
                          initialDelaySeconds = 20;
                          periodSeconds = 10;
                          timeoutSeconds = 5;
                          failureThreshold = 6;
                        };

                        livenessProbe = {
                          httpGet = {
                            path = "/";
                            port = "http";
                          };
                          initialDelaySeconds = 60;
                          periodSeconds = 30;
                          timeoutSeconds = 5;
                          failureThreshold = 3;
                        };

                        volumeMounts = [
                          {
                            mountPath = "/opt/xyops/data";
                            name = "data";
                            subPath = "data";
                          }
                          {
                            mountPath = "/opt/xyops/conf";
                            name = "data";
                            subPath = "conf";
                          }
                        ];
                      }
                    ];

                    volumes = [ cfg.volumes.data.volume ];
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
                        pathType = "ImplementationSpecific";
                        backend.service = {
                          inherit name;
                          port.name = "http";
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
                  name = "http";
                  port = 5522;
                  protocol = "TCP";
                  targetPort = "http";
                }
              ];
            };

          };
      };
}
