{ ... }:
{
  flake.nixidyApps.nostrarchives =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      name = "nostrarchives";
      labels = {
        "app.kubernetes.io/name" = name;
        "app.kubernetes.io/instance" = name;
      };

      db-secret = "nostrarchives-db";
      redis-secret = "nostrarchives-redis";
      enc = pkgs.lib.escapeURL;
      databaseUrl =
        cfg:
        "postgresql://${enc cfg.database.username}:${enc cfg.database.password}@${cfg.database.host}:${toString cfg.database.port}/${cfg.database.name}";
      redisUrl =
        cfg:
        if cfg.redis.password != "" then
          "redis://:${enc cfg.redis.password}@${cfg.redis.host}:${toString cfg.redis.port}"
        else
          "redis://${cfg.redis.host}:${toString cfg.redis.port}";

      nixExpr = ''
        (builtins.getFlake "github:duck1123/k3s-fleetops").packages.x86_64-linux.nostrarchives-api-bundle
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
      {
        inherit name;
        uses-ingress = true;
        uses-database = true;

        extraOptions = {
          service.port = mkOption {
            description = mdDoc "REST API listen port";
            type = types.int;
            default = 8000;
          };

          relayDomain = mkOption {
            description = mdDoc "Tailscale domain for the NIP-50 WebSocket relay (port 8001). Empty string disables the relay ingress.";
            type = types.str;
            default = "";
          };

          redis = {
            host = mkOption {
              description = mdDoc "Redis service host";
              type = types.str;
              default = "redis.redis";
            };
            port = mkOption {
              description = mdDoc "Redis port";
              type = types.int;
              default = 6379;
            };
            password = mkOption {
              description = mdDoc "Redis password (empty = no auth)";
              type = types.str;
              default = "";
            };
          };
        };

        sopsSecrets =
          cfg:
          optionalAttrs (cfg.database.password != "") {
            ${db-secret}.DATABASE_URL = databaseUrl cfg;
          }
          // optionalAttrs (cfg.redis.password != "") {
            ${redis-secret}.REDIS_URL = redisUrl cfg;
          };

        extraResources =
          cfg:
          let
            envVars = [
              {
                name = "LISTEN_ADDR";
                value = "0.0.0.0:${toString cfg.service.port}";
              }
              {
                name = "WS_LISTEN_ADDR";
                value = "0.0.0.0:8001";
              }
              {
                name = "SCHEDULER_WS_LISTEN_ADDR";
                value = "0.0.0.0:8002";
              }
              {
                name = "INDEXER_WS_LISTEN_ADDR";
                value = "0.0.0.0:8003";
              }
              {
                name = "SSL_CERT_FILE";
                value = "/nix/var/result/etc/ssl/certs/ca-bundle.crt";
              }
            ]
            ++ optionals (cfg.database.password != "") [
              {
                name = "DATABASE_URL";
                valueFrom.secretKeyRef = {
                  name = db-secret;
                  key = "DATABASE_URL";
                };
              }
            ]
            ++ optionals (cfg.redis.password != "") [
              {
                name = "REDIS_URL";
                valueFrom.secretKeyRef = {
                  name = redis-secret;
                  key = "REDIS_URL";
                };
              }
            ];
          in
          {
            deployments.${name}.spec = {
              selector.matchLabels = labels;
              template = {
                metadata.labels = labels;
                spec = {
                  containers = [
                    {
                      inherit name;
                      image = "ghcr.io/lillecarl/nix-csi/scratch:1.0.1";
                      command = [ "nostr-api" ];
                      env = envVars;
                      ports = [
                        {
                          containerPort = cfg.service.port;
                          name = "http";
                          protocol = "TCP";
                        }
                        {
                          containerPort = 8001;
                          name = "ws-search";
                          protocol = "TCP";
                        }
                        {
                          containerPort = 8002;
                          name = "ws-scheduler";
                          protocol = "TCP";
                        }
                        {
                          containerPort = 8003;
                          name = "ws-indexer";
                          protocol = "TCP";
                        }
                      ];
                      livenessProbe = {
                        httpGet = {
                          path = "/health";
                          port = cfg.service.port;
                        };
                        # Allow time for the first nix-csi build (Rust compile can be slow)
                        initialDelaySeconds = 600;
                        periodSeconds = 30;
                        timeoutSeconds = 5;
                        failureThreshold = 3;
                      };
                      readinessProbe = {
                        httpGet = {
                          path = "/health";
                          port = cfg.service.port;
                        };
                        initialDelaySeconds = 60;
                        periodSeconds = 10;
                        timeoutSeconds = 5;
                        failureThreshold = 6;
                      };
                      volumeMounts = [
                        {
                          name = "nix";
                          mountPath = "/nix";
                          subPath = "nix";
                        }
                      ];
                    }
                  ];
                  volumes = [
                    {
                      name = "nix";
                      csi = {
                        driver = "nix.csi.store";
                        volumeAttributes.nixExpr = nixExpr;
                      };
                    }
                  ];
                };
              };
            };

            ingresses = {
              ${name}.spec = with cfg.ingress; {
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
                tls = [ { hosts = [ domain ]; } ];
              };
            }
            // optionalAttrs (cfg.relayDomain != "") {
              "${name}-relay".spec = {
                ingressClassName = cfg.ingress.ingressClassName;
                rules = [
                  {
                    host = cfg.relayDomain;
                    http.paths = [
                      {
                        path = "/";
                        pathType = "ImplementationSpecific";
                        backend.service = {
                          inherit name;
                          port.name = "ws-search";
                        };
                      }
                    ];
                  }
                ];
                tls = [ { hosts = [ cfg.relayDomain ]; } ];
              };
            };

            services.${name}.spec = {
              selector = labels;
              ports = [
                {
                  name = "http";
                  port = cfg.service.port;
                  protocol = "TCP";
                  targetPort = "http";
                }
                {
                  name = "ws-search";
                  port = 8001;
                  protocol = "TCP";
                  targetPort = "ws-search";
                }
                {
                  name = "ws-scheduler";
                  port = 8002;
                  protocol = "TCP";
                  targetPort = "ws-scheduler";
                }
                {
                  name = "ws-indexer";
                  port = 8003;
                  protocol = "TCP";
                  targetPort = "ws-indexer";
                }
              ];
            };
          };
      };
}
