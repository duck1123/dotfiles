{ ... }:
{
  flake.nixidyApps.gramps-web =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      name = "gramps-web";
      redis-secret = "gramps-web-redis";
      enc = pkgs.lib.escapeURL;
      redisDsn =
        cfg: db:
        "redis://:${enc cfg.redis.password}@${cfg.redis.host}:${toString cfg.redis.port}/${toString db}";
      redisEnvVar =
        cfg: envName: key: plainDb:
        if cfg.redis.password != "" then
          {
            name = envName;
            valueFrom.secretKeyRef = {
              name = redis-secret;
              inherit key;
            };
          }
        else
          {
            name = envName;
            value = "redis://${cfg.redis.host}:${toString cfg.redis.port}/${toString plainDb}";
          };
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

        # Shape only -- no volumeHandle here, that's environment-specific (see
        # env/dev/gramps-web.nix and docs/pinned-volumes.md).
        volumes = cfg: {
          users.size = "100Mi";
          index.size = "1Gi";
          thumbcache.size = "2Gi";
          cache.size = "2Gi";
          secret.size = "50Mi";
          db.size = "2Gi";
          media.size = "20Gi";
        };

        sopsSecrets =
          cfg:
          optionalAttrs (cfg.redis.password != "") {
            ${redis-secret} = {
              brokerUrl = redisDsn cfg cfg.redis.celeryDbIndex;
              ratelimitUrl = redisDsn cfg cfg.redis.ratelimitDbIndex;
            };
          };

        extraOptions = {
          image = mkOption {
            description = mdDoc "Docker image to use (combined frontend+API image). Pin a release tag, not `:latest`.";
            type = types.str;
            default = "ghcr.io/gramps-project/grampsweb:26.7.0";
          };

          treeName = mkOption {
            description = mdDoc "Name of the Gramps family tree to create on first run (GRAMPSWEB_TREE). Ignored once a tree already exists in the db volume.";
            type = types.str;
            default = "Gramps Web";
          };

          redis = {
            host = mkOption {
              description = mdDoc "The Redis host (Celery broker/result backend + API rate-limit storage)";
              type = types.str;
              default = "redis.redis";
            };

            port = mkOption {
              description = mdDoc "The Redis port";
              type = types.int;
              default = 6379;
            };

            password = mkOption {
              description = mdDoc "The Redis password";
              type = types.str;
              default = "";
            };

            # Dedicated db indices (not 0/2/3/4, already used by paperless-ngx/immich,
            # securo, and superset respectively -- see incident-tube-archivist-celery-pickle-crashloop
            # in memory for why colliding Celery apps on the same db is dangerous: a
            # differently-configured app's messages land in this one's queue and get
            # rejected as untrusted content, or vice versa) so gramps-web's Celery
            # broker/results and rate-limit storage never collide with another app's keys.
            celeryDbIndex = mkOption {
              description = mdDoc "Redis database index for gramps-web's Celery broker/result backend";
              type = types.int;
              default = 5;
            };

            ratelimitDbIndex = mkOption {
              description = mdDoc "Redis database index for gramps-web's API rate-limit storage";
              type = types.int;
              default = 6;
            };
          };

          service.port = mkOption {
            description = mdDoc "The service port";
            type = types.int;
            default = 5000;
          };

          gunicornWorkers = mkOption {
            description = mdDoc ''
              Gunicorn worker process count (image defaults to 8 via `GUNICORN_NUM_WORKERS`).
              Gramps' sqlite backend has no cross-process write coordination -- multiple
              gunicorn workers hitting the same grampsdb sqlite file at once fail writes
              with `sqlite3.OperationalError: database is locked`. Keep at 1 unless/until
              the tree is moved to a real database backend.
            '';
            type = types.int;
            default = 1;
          };
        };

        extraResources =
          cfg:
          let
            sharedEnv = [
              {
                name = "GRAMPSWEB_TREE";
                value = cfg.treeName;
              }
              {
                name = "GUNICORN_NUM_WORKERS";
                value = toString cfg.gunicornWorkers;
              }
              (redisEnvVar cfg "GRAMPSWEB_CELERY_CONFIG__broker_url" "brokerUrl" cfg.redis.celeryDbIndex)
              (redisEnvVar cfg "GRAMPSWEB_CELERY_CONFIG__result_backend" "brokerUrl" cfg.redis.celeryDbIndex)
              (redisEnvVar cfg "GRAMPSWEB_RATELIMIT_STORAGE_URI" "ratelimitUrl" cfg.redis.ratelimitDbIndex)
            ];
            sharedVolumeMounts = [
              {
                mountPath = "/app/users";
                name = "users";
              }
              {
                mountPath = "/app/indexdir";
                name = "index";
              }
              {
                mountPath = "/app/thumbnail_cache";
                name = "thumbcache";
              }
              {
                mountPath = "/app/cache";
                name = "cache";
              }
              {
                mountPath = "/app/secret";
                name = "secret";
              }
              {
                mountPath = "/root/.gramps/grampsdb";
                name = "db";
              }
              {
                mountPath = "/app/media";
                name = "media";
              }
              {
                mountPath = "/tmp";
                name = "tmp";
              }
            ];
            sharedVolumes = [
              cfg.volumes.users.volume
              cfg.volumes.index.volume
              cfg.volumes.thumbcache.volume
              cfg.volumes.cache.volume
              cfg.volumes.secret.volume
              cfg.volumes.db.volume
              cfg.volumes.media.volume
              {
                name = "tmp";
                emptyDir = { };
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
                # Both containers share pod-local ReadWriteOnce PVCs -- can't roll a
                # second pod alongside the first while they're still attached.
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
                        env = sharedEnv;
                        ports = [
                          {
                            containerPort = 5000;
                            name = "http";
                            protocol = "TCP";
                          }
                        ];
                        volumeMounts = sharedVolumeMounts;
                      }
                      {
                        name = "${name}-celery";
                        image = cfg.image;
                        imagePullPolicy = "IfNotPresent";
                        # `args`, not `command` -- k8s `command` overrides the image's
                        # ENTRYPOINT (docker-entrypoint.sh), which is what generates/exports
                        # GRAMPSWEB_SECRET_KEY. `args` overrides CMD only, same as compose's
                        # `command:` does, so the entrypoint still runs before this.
                        args = [
                          "celery"
                          "-A"
                          "gramps_webapi.celery"
                          "worker"
                          "--loglevel=INFO"
                          "--concurrency=2"
                        ];
                        env = sharedEnv;
                        volumeMounts = sharedVolumeMounts;
                      }
                    ];
                    volumes = sharedVolumes;
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
          };
      };
}
