{ ... }:
{
  flake.nixidyApps.nocodb =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      name = "nocodb";
      jwt-secret = "nocodb-jwt-secret";
      db-secret = "nocodb-database";
      redis-secret = "nocodb-redis-url";
      storage-work-volume = "nocodb-env-work";
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

        # `pvcName = "${name}-data"` (not the usual "${name}-${name}-<key>"
        # convention). Shape only -- no volumeHandle here, that's
        # environment-specific (see env/dev/nocodb.nix and docs/pinned-volumes.md).
        volumes = cfg: {
          data = {
            pvcName = "${name}-data";
            size = "5Gi";
          };
        };

        sopsSecrets =
          cfg:
          optionalAttrs (cfg.auth.jwtSecret != "") {
            ${jwt-secret}.NC_AUTH_JWT_SECRET = cfg.auth.jwtSecret;
          }
          // optionalAttrs (cfg.database.password != "") {
            ${db-secret}.password = cfg.database.password;
          }
          // optionalAttrs (cfg.redis.password != "") {
            ${redis-secret}.password = cfg.redis.password;
          }
          // optionalAttrs (cfg.storage.enable && cfg.storage.accessKey != "") {
            "nocodb-storage" = {
              accessKey = cfg.storage.accessKey;
              secretKey = cfg.storage.secretKey;
            };
          };

        extraOptions = {
          image = mkOption {
            description = mdDoc "NocoDB Docker image";
            type = types.str;
            default = "nocodb/nocodb:2026.08.2";
          };

          service.port = mkOption {
            description = mdDoc "Service port";
            type = types.int;
            default = 8080;
          };

          auth.jwtSecret = mkOption {
            description = mdDoc "JWT secret for auth tokens (generate: openssl rand -hex 32)";
            type = types.str;
            default = "";
          };

          redis = {
            host = mkOption {
              description = mdDoc "Redis host (cluster service)";
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

          storage = {
            backend = mkOption {
              description = mdDoc "S3-compatible storage backend: garage, rustfs, or minio (label only -- doesn't affect resource generation, see endpoint/region/accessKey/secretKey below)";
              type = types.enum [
                "garage"
                "rustfs"
                "minio"
              ];
              default = "garage";
            };
            enable = mkOption {
              description = mdDoc "Enable S3/MinIO storage for attachments";
              type = types.bool;
              default = false;
            };
            bucketName = mkOption {
              description = mdDoc "S3/MinIO bucket name";
              type = types.str;
              default = "";
            };
            endpoint = mkOption {
              description = mdDoc ''
                S3 endpoint. Must be externally resolvable (e.g. Garage's ingress
                domain, not the in-cluster `garage.garage` ClusterIP) -- NocoDB
                hands attachment URLs to the browser as S3 presigned URLs, so the
                endpoint has to work from outside the cluster too.
              '';
              type = types.str;
              default = "";
            };
            region = mkOption {
              description = mdDoc "S3 region";
              type = types.str;
              default = "us-east-1";
            };
            accessKey = mkOption {
              description = mdDoc "S3 access key";
              type = types.str;
              default = "";
            };
            secretKey = mkOption {
              description = mdDoc "S3 secret key";
              type = types.str;
              default = "";
            };
            forcePathStyle = mkOption {
              description = mdDoc "Force path-style for MinIO";
              type = types.bool;
              default = true;
            };
          };

          publicUrl = mkOption {
            description = mdDoc "Public URL (e.g. https://nocodb.example.com)";
            type = types.str;
            default = "";
          };

          disableTelemetry = mkOption {
            description = mdDoc "Disable telemetry";
            type = types.bool;
            default = true;
          };

          replicas = mkOption {
            description = mdDoc "Number of replicas";
            type = types.int;
            default = 1;
          };

          # https://nocodb.com/docs/self-hosting/environment-variables — defaults to false (blocks private IPs).
          allowLocalExternalDatabases = mkOption {
            description = mdDoc ''
              Set `NC_ALLOW_LOCAL_EXTERNAL_DBS=true` so the "external connection" UI can use hosts that resolve to
              private/cluster addresses (e.g. `postgresql.postgresql` → ClusterIP). NocoDB blocks these by default (SSRF protection).
            '';
            type = types.bool;
            default = false;
          };
        };

        extraResources = cfg: {
          deployments.${name} = {
            metadata.labels = {
              "app.kubernetes.io/instance" = name;
              "app.kubernetes.io/name" = name;
              "app.kubernetes.io/version" = "0.301.4";
            };

            spec = {
              replicas = cfg.replicas;

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

                  initContainers = optional (cfg.database.password != "" || cfg.redis.password != "") {
                    name = "build-connection-urls";
                    image = "python:3-alpine";
                    imagePullPolicy = "IfNotPresent";
                    command = [
                      "python3"
                      "-c"
                      ''
                        import urllib.parse
                        import os

                        if os.path.exists("/secrets-db/password"):
                            with open("/secrets-db/password") as f:
                                pwd = f.read()
                            user = os.environ.get("PGUSER", "nocodb")
                            host = os.environ.get("PGHOST", "postgresql.postgresql")
                            port = os.environ.get("PGPORT", "5432")
                            db = os.environ.get("PGDATABASE", "nocodb")
                            enc = urllib.parse.quote(pwd, safe="")
                            nc_db = f"pg://{host}:{port}?u={user}&p={enc}&d={db}"
                            with open("/work/NC_DB", "w") as f:
                                f.write(nc_db)

                        if os.path.exists("/secrets-redis/password"):
                            with open("/secrets-redis/password") as f:
                                pwd = f.read()
                            host = os.environ.get("REDIS_HOST", "redis.redis")
                            port = os.environ.get("REDIS_PORT", "6379")
                            enc = urllib.parse.quote(pwd, safe="")
                            url = f"redis://:{enc}@{host}:{port}/0"
                            with open("/work/NC_REDIS_URL", "w") as f:
                                f.write(url)
                      ''
                    ];
                    env = [
                      {
                        name = "PGUSER";
                        value = cfg.database.username;
                      }
                      {
                        name = "PGHOST";
                        value = cfg.database.host;
                      }
                      {
                        name = "PGPORT";
                        value = toString cfg.database.port;
                      }
                      {
                        name = "PGDATABASE";
                        value = cfg.database.name;
                      }
                      {
                        name = "REDIS_HOST";
                        value = cfg.redis.host;
                      }
                      {
                        name = "REDIS_PORT";
                        value = toString cfg.redis.port;
                      }
                    ];
                    volumeMounts = [
                      {
                        mountPath = "/work";
                        name = storage-work-volume;
                      }
                    ]
                    ++ optional (cfg.database.password != "") {
                      mountPath = "/secrets-db";
                      name = "db-secret";
                    }
                    ++ optional (cfg.redis.password != "") {
                      mountPath = "/secrets-redis";
                      name = "redis-secret";
                    };
                  };

                  containers = [
                    (
                      {
                        inherit name;
                        image = cfg.image;
                        imagePullPolicy = "IfNotPresent";
                        env = [
                          {
                            name = "PORT";
                            value = toString cfg.service.port;
                          }
                          {
                            name = "NC_TOOL_DIR";
                            value = "/usr/app/data";
                          }
                          {
                            name = "NC_DISABLE_TELE";
                            value = if cfg.disableTelemetry then "true" else "false";
                          }
                        ]
                        ++ optional cfg.allowLocalExternalDatabases {
                          name = "NC_ALLOW_LOCAL_EXTERNAL_DBS";
                          value = "true";
                        }
                        ++ optional (cfg.auth.jwtSecret != "") {
                          name = "NC_AUTH_JWT_SECRET";
                          valueFrom.secretKeyRef = {
                            name = jwt-secret;
                            key = "NC_AUTH_JWT_SECRET";
                          };
                        }
                        ++ optional (cfg.publicUrl != "") {
                          name = "NC_PUBLIC_URL";
                          value = cfg.publicUrl;
                        }
                        # NC_DB and NC_REDIS_URL: from init container when passwords in secrets
                        # Redis without password: direct env
                        ++ optional (cfg.redis.password == "" && cfg.redis.host != "") {
                          name = "NC_REDIS_URL";
                          value = "redis://${cfg.redis.host}:${toString cfg.redis.port}/0";
                        }
                        # S3/MinIO storage
                        ++ optionals (cfg.storage.enable && cfg.storage.bucketName != "") [
                          {
                            name = "NC_S3_BUCKET_NAME";
                            value = cfg.storage.bucketName;
                          }
                          {
                            name = "NC_S3_REGION";
                            value = cfg.storage.region;
                          }
                          {
                            name = "NC_S3_ENDPOINT";
                            value = cfg.storage.endpoint;
                          }
                          {
                            name = "NC_S3_FORCE_PATH_STYLE";
                            value = if cfg.storage.forcePathStyle then "true" else "false";
                          }
                        ]
                        ++ optionals (cfg.storage.enable && cfg.storage.accessKey != "") [
                          {
                            name = "NC_S3_ACCESS_KEY";
                            valueFrom.secretKeyRef = {
                              name = "nocodb-storage";
                              key = "accessKey";
                            };
                          }
                          {
                            name = "NC_S3_ACCESS_SECRET";
                            valueFrom.secretKeyRef = {
                              name = "nocodb-storage";
                              key = "secretKey";
                            };
                          }
                        ];
                        volumeMounts = [
                          {
                            mountPath = "/usr/app/data";
                            name = "data";
                          }
                        ]
                        ++ optional (cfg.database.password != "" || cfg.redis.password != "") {
                          mountPath = "/work";
                          name = storage-work-volume;
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
                          initialDelaySeconds = 15;
                          periodSeconds = 10;
                          timeoutSeconds = 5;
                          successThreshold = 1;
                          failureThreshold = 5;
                        };
                        livenessProbe = {
                          httpGet = {
                            path = "/api/v1/health";
                            port = cfg.service.port;
                          };
                          initialDelaySeconds = 30;
                          periodSeconds = 30;
                          timeoutSeconds = 5;
                          successThreshold = 1;
                          failureThreshold = 5;
                        };
                      }
                      // optionalAttrs (cfg.database.password != "" || cfg.redis.password != "") {
                        command = [
                          "/bin/sh"
                          "-c"
                          ''
                            [ -f /work/NC_DB ] && export NC_DB=$(cat /work/NC_DB)
                            [ -f /work/NC_REDIS_URL ] && export NC_REDIS_URL=$(cat /work/NC_REDIS_URL)
                            exec /usr/src/appEntry/start.sh
                          ''
                        ];
                      }
                    )
                  ];

                  volumes = [
                    cfg.volumes.data.volume
                  ]
                  ++ optional (cfg.database.password != "" || cfg.redis.password != "") {
                    name = storage-work-volume;
                    emptyDir = { };
                  }
                  ++ optional (cfg.database.password != "") {
                    name = "db-secret";
                    secret.secretName = db-secret;
                  }
                  ++ optional (cfg.redis.password != "") {
                    name = "redis-secret";
                    secret.secretName = redis-secret;
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
                  secretName = tls.secretName;
                }
              ];
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
        };
      };
}
