{ ... }:
{
  flake.nixidyApps.affine =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      name = "affine";
      db-secret = "affine-database-url";
      redis-secret = "affine-redis-password";
      databaseUrl =
        cfg:
        "postgresql://${cfg.database.username}:${cfg.database.password}@${cfg.database.host}:${toString cfg.database.port}/${cfg.database.name}";
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
        uses-database = true;

        sopsSecrets =
          cfg:
          optionalAttrs (cfg.database.password != "") {
            ${db-secret}.DATABASE_URL = databaseUrl cfg;
          }
          // optionalAttrs (cfg.redis.password != "") {
            ${redis-secret}.REDIS_SERVER_PASSWORD = cfg.redis.password;
          };

        # `pvcName` overrides since these predate the "${name}-${name}-<key>"
        # convention. Shape only -- no volumeHandle here, that's
        # environment-specific (see env/dev/affine.nix and docs/pinned-volumes.md).
        volumes = cfg: {
          storage = {
            pvcName = "${name}-storage";
            size = "10Gi";
          };
          affine-config = {
            pvcName = "${name}-config";
            size = "1Gi";
          };
        };

        extraOptions = {
          image = mkOption {
            description = mdDoc "AFFiNE Docker image (ghcr.io/toeverything/affine)";
            type = types.str;
            default = "ghcr.io/toeverything/affine:stable";
          };

          redis = {
            host = mkOption {
              description = mdDoc "Redis host (cluster service DNS)";
              type = types.str;
              default = "redis.redis";
            };
            port = mkOption {
              description = mdDoc "Redis port";
              type = types.int;
              default = 6379;
            };
            password = mkOption {
              description = mdDoc "Redis password";
              type = types.str;
              default = "";
            };
          };

          serverExternalUrl = mkOption {
            description = mdDoc "Public-facing URL for AFFiNE (e.g. https://affine.example.com). Sets AFFINE_SERVER_EXTERNAL_URL.";
            type = types.str;
            default = "";
          };
        };

        extraResources = cfg: {
          # Migration job — runs self-host-predeploy.js on each sync, after secrets are created (wave 1)
          # Using Sync phase (not PreSync) so the secret exists before the job runs.
          jobs."${name}-migration" = {
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
                    name = "migration";
                    image = cfg.image;
                    imagePullPolicy = "IfNotPresent";
                    command = [
                      "sh"
                      "-c"
                      "node ./scripts/self-host-predeploy.js"
                    ];
                    env = [
                      {
                        name = "DATABASE_URL";
                        valueFrom.secretKeyRef = {
                          name = db-secret;
                          key = "DATABASE_URL";
                        };
                      }
                      {
                        name = "REDIS_SERVER_HOST";
                        value = cfg.redis.host;
                      }
                      {
                        name = "REDIS_SERVER_PORT";
                        value = toString cfg.redis.port;
                      }
                    ]
                    ++ optional (cfg.redis.password != "") {
                      name = "REDIS_SERVER_PASSWORD";
                      valueFrom.secretKeyRef = {
                        name = redis-secret;
                        key = "REDIS_SERVER_PASSWORD";
                      };
                    };
                  }
                ];
              };
            };
          };

          deployments.${name} = {
            metadata.annotations."argocd.argoproj.io/sync-wave" = "2";
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
                  containers = [
                    {
                      inherit name;
                      image = cfg.image;
                      imagePullPolicy = "IfNotPresent";
                      ports = [
                        {
                          containerPort = 3010;
                          name = "http";
                          protocol = "TCP";
                        }
                      ];
                      env = [
                        {
                          name = "REDIS_SERVER_HOST";
                          value = cfg.redis.host;
                        }
                        {
                          name = "REDIS_SERVER_PORT";
                          value = toString cfg.redis.port;
                        }
                        {
                          name = "AFFINE_INDEXER_ENABLED";
                          value = "false";
                        }
                      ]
                      ++ optional (cfg.database.password != "") {
                        name = "DATABASE_URL";
                        valueFrom.secretKeyRef = {
                          name = db-secret;
                          key = "DATABASE_URL";
                        };
                      }
                      ++ optional (cfg.redis.password != "") {
                        name = "REDIS_SERVER_PASSWORD";
                        valueFrom.secretKeyRef = {
                          name = redis-secret;
                          key = "REDIS_SERVER_PASSWORD";
                        };
                      }
                      ++ optional (cfg.serverExternalUrl != "") {
                        name = "AFFINE_SERVER_EXTERNAL_URL";
                        value = cfg.serverExternalUrl;
                      };
                      volumeMounts = [
                        {
                          mountPath = "/root/.affine/storage";
                          name = "storage";
                        }
                        {
                          mountPath = "/root/.affine/config";
                          name = "affine-config";
                        }
                      ];
                      readinessProbe = {
                        httpGet = {
                          path = "/";
                          port = 3010;
                        };
                        initialDelaySeconds = 30;
                        periodSeconds = 10;
                        timeoutSeconds = 5;
                        successThreshold = 1;
                        failureThreshold = 6;
                      };
                      livenessProbe = {
                        httpGet = {
                          path = "/";
                          port = 3010;
                        };
                        initialDelaySeconds = 60;
                        periodSeconds = 30;
                        timeoutSeconds = 10;
                        successThreshold = 1;
                        failureThreshold = 5;
                      };
                    }
                  ];
                  volumes = [
                    cfg.volumes.storage.volume
                    cfg.volumes.affine-config.volume
                  ];
                };
              };
            };
          };

          services.${name}.spec = {
            ports = [
              {
                name = "http";
                port = 3010;
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

          ingresses.${name}.spec = with cfg.ingress; {
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
                secretName = tls.secretName;
              }
            ];
          };

        };
      };
}
