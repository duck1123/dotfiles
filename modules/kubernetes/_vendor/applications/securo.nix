{ ... }:
{
  flake.nixidyApps.securo =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      name = "securo";
      db-secret = "securo-database";
      redis-secret = "securo-redis";
      secret-key-secret = "securo-secret-key";
      agents-jwt-secret = "securo-agents-mcp-jwt";
      pluggy-secret = "securo-pluggy";
      enc = pkgs.lib.escapeURL;
      databaseDsn =
        cfg:
        "postgresql+asyncpg://${enc cfg.database.username}:${enc cfg.database.password}@${cfg.database.host}:${toString cfg.database.port}/${cfg.database.name}";
      redisDsn =
        cfg:
        "redis://:${enc cfg.redis.password}@${cfg.redis.host}:${toString cfg.redis.port}/${toString cfg.redis.dbIndex}";
      boolStr = b: if b then "true" else "false";

      # Backend, celery-worker, celery-beat and mcp-server all run the same
      # image with the same env/volumes in upstream's compose file (an
      # `x-backend-base` YAML anchor) -- only `command` differs per role.
      backendEnv =
        cfg:
        [
          {
            name = "DEBUG";
            value = "false";
          }
          {
            name = "FRONTEND_URL";
            value = "https://${cfg.ingress.domain}";
          }
          {
            name = "WEBAUTHN_RP_ID";
            value = cfg.ingress.domain;
          }
          {
            name = "SIMPLEFIN_ENABLED";
            value = boolStr cfg.simplefin.enable;
          }
          {
            name = "SIMPLEFIN_API_URL";
            value = cfg.simplefin.apiUrl;
          }
          {
            name = "OPENEXCHANGERATES_APP_ID";
            value = cfg.openExchangeRatesAppId;
          }
          {
            name = "TESOURO_DIRETO_ENABLED";
            value = boolStr cfg.tesouroDiretoEnabled;
          }
          {
            name = "STORAGE_LOCAL_PATH";
            value = "/app/data/attachments";
          }
          {
            name = "AGENTS_KNOWLEDGE_STORAGE_PATH";
            value = "/app/data/agent_knowledge";
          }
          {
            name = "AGENTS_ENABLED";
            value = boolStr cfg.agents.enable;
          }
        ]
        ++ optional (cfg.database.password != "") {
          name = "DATABASE_URL";
          valueFrom.secretKeyRef = {
            name = db-secret;
            key = "url";
          };
        }
        ++ optional (cfg.secretKey != "") {
          name = "SECRET_KEY";
          valueFrom.secretKeyRef = {
            name = secret-key-secret;
            key = "key";
          };
        }
        ++ (
          if cfg.redis.password != "" then
            [
              {
                name = "REDIS_URL";
                valueFrom.secretKeyRef = {
                  name = redis-secret;
                  key = "url";
                };
              }
            ]
          else
            [
              {
                name = "REDIS_URL";
                value = "redis://${cfg.redis.host}:${toString cfg.redis.port}/${toString cfg.redis.dbIndex}";
              }
            ]
        )
        ++ optionals (cfg.pluggy.clientId != "" && cfg.pluggy.clientSecret != "") [
          {
            name = "PLUGGY_CLIENT_ID";
            valueFrom.secretKeyRef = {
              name = pluggy-secret;
              key = "clientId";
            };
          }
          {
            name = "PLUGGY_CLIENT_SECRET";
            valueFrom.secretKeyRef = {
              name = pluggy-secret;
              key = "clientSecret";
            };
          }
        ]
        ++ optionals cfg.agents.enable (
          [
            {
              name = "AGENTS_BUILTIN_MCP_URL";
              value = "http://${name}-mcp.${cfg.namespace}:8765/mcp";
            }
            {
              name = "AGENTS_DEFAULT_PROVIDER";
              value = cfg.agents.defaultProvider;
            }
            {
              name = "AGENTS_OLLAMA_BASE_URL";
              value = cfg.agents.ollamaBaseUrl;
            }
            {
              name = "AGENTS_EMBEDDING_PROVIDER";
              value = cfg.agents.embeddingProvider;
            }
          ]
          ++ optional (cfg.agents.mcpJwtSecret != "") {
            name = "AGENTS_MCP_JWT_SECRET";
            valueFrom.secretKeyRef = {
              name = agents-jwt-secret;
              key = "secret";
            };
          }
        );

      backendVolumeMounts = cfg: [
        {
          mountPath = "/app/data/attachments";
          name = "attachments";
        }
        {
          mountPath = "/app/data/agent_knowledge";
          name = "agent-knowledge";
        }
        {
          mountPath = "/app/data/embedding_models";
          name = "embedding-models";
        }
      ];

      backendVolumes = cfg: [
        cfg.volumes.attachments.volume
        cfg.volumes.agent-knowledge.volume
        cfg.volumes.embedding-models.volume
      ];

      mkBackendDeployment =
        cfg:
        {
          suffix,
          command,
          ports ? [ ],
        }:
        {
          metadata.labels = {
            "app.kubernetes.io/instance" = "${name}-${suffix}";
            "app.kubernetes.io/name" = "${name}-${suffix}";
          };

          spec = {
            replicas = 1;
            strategy.type = "Recreate";
            selector.matchLabels = {
              "app.kubernetes.io/instance" = "${name}-${suffix}";
              "app.kubernetes.io/name" = "${name}-${suffix}";
            };

            template = {
              metadata.labels = {
                "app.kubernetes.io/instance" = "${name}-${suffix}";
                "app.kubernetes.io/name" = "${name}-${suffix}";
              };

              spec = {
                automountServiceAccountToken = true;
                serviceAccountName = "default";

                containers = [
                  {
                    name = "${name}-${suffix}";
                    image = "ghcr.io/securo-finance/securo-backend:${cfg.image.tag}";
                    imagePullPolicy = "IfNotPresent";
                    inherit command;
                    env = backendEnv cfg;
                    volumeMounts = backendVolumeMounts cfg;
                    ports = ports;
                  }
                ];

                volumes = backendVolumes cfg;
              };
            };
          };
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
        uses-database = true;

        volumes = cfg: {
          attachments.size = "5Gi";
          agent-knowledge.size = "2Gi";
          embedding-models.size = "5Gi";
        };

        sopsSecrets =
          cfg:
          optionalAttrs (cfg.database.password != "") {
            ${db-secret} = {
              password = cfg.database.password;
              url = databaseDsn cfg;
            };
          }
          // optionalAttrs (cfg.redis.password != "") {
            ${redis-secret}.url = redisDsn cfg;
          }
          // optionalAttrs (cfg.secretKey != "") {
            ${secret-key-secret}.key = cfg.secretKey;
          }
          // optionalAttrs (cfg.pluggy.clientId != "" && cfg.pluggy.clientSecret != "") {
            ${pluggy-secret} = {
              inherit (cfg.pluggy) clientId clientSecret;
            };
          }
          // optionalAttrs (cfg.agents.enable && cfg.agents.mcpJwtSecret != "") {
            ${agents-jwt-secret}.secret = cfg.agents.mcpJwtSecret;
          };

        extraOptions = {
          image.tag = mkOption {
            description = mdDoc "The securo-backend/securo-frontend docker image tag (both track the same release version)";
            type = types.str;
            default = "0.15.1";
          };

          secretKey = mkOption {
            description = mdDoc "Value for SECRET_KEY (backend session/token signing key -- generate with `openssl rand -hex 32`). Do not leave empty in a real deployment: an empty value lets the container fall back to an ephemeral key that invalidates sessions on every restart.";
            type = types.str;
            default = "";
          };

          redis = {
            host = mkOption {
              description = mdDoc "The Redis host";
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

            dbIndex = mkOption {
              description = mdDoc "The Redis database index";
              type = types.int;
              default = 0;
            };
          };

          simplefin = {
            enable = mkOption {
              description = mdDoc "Enable SimpleFIN bank sync (read-only, no API key required -- see Settings -> Admin -> Bank Sync in Securo once a bridge account is linked at bridge.simplefin.org)";
              type = types.bool;
              default = true;
            };

            apiUrl = mkOption {
              description = mdDoc "SimpleFIN bridge URL. The default is the sandbox; point at https://bridge.simplefin.org for real banks.";
              type = types.str;
              default = "https://beta-bridge.simplefin.org";
            };
          };

          openExchangeRatesAppId = mkOption {
            description = mdDoc "Open Exchange Rates app ID for automatic multi-currency conversion. Empty = cross-currency amounts fall back to a 1:1 rate.";
            type = types.str;
            default = "";
          };

          tesouroDiretoEnabled = mkOption {
            description = mdDoc "Enable Brazilian Treasury (Tesouro Direto) lookups";
            type = types.bool;
            default = false;
          };

          pluggy = {
            clientId = mkOption {
              description = mdDoc "Pluggy client ID (Brazilian bank sync)";
              type = types.str;
              default = "";
            };

            clientSecret = mkOption {
              description = mdDoc "Pluggy client secret";
              type = types.str;
              default = "";
            };
          };

          agents = {
            enable = mkOption {
              description = mdDoc "Enable Securo's self-hosted AI agents/MCP server (off by default -- costs nothing when off). Providers are configured from Settings -> AI Agents in the app itself once enabled.";
              type = types.bool;
              default = false;
            };

            defaultProvider = mkOption {
              description = mdDoc "AGENTS_DEFAULT_PROVIDER";
              type = types.str;
              default = "ollama";
            };

            ollamaBaseUrl = mkOption {
              description = mdDoc "AGENTS_OLLAMA_BASE_URL";
              type = types.str;
              default = "http://ollama:11434";
            };

            embeddingProvider = mkOption {
              description = mdDoc "AGENTS_EMBEDDING_PROVIDER";
              type = types.str;
              default = "native";
            };

            mcpJwtSecret = mkOption {
              description = mdDoc "Signing secret for JWTs minted from Settings -> AI Agents -> External MCP access (lets external MCP clients like Claude Desktop query Securo). Empty = the image's insecure built-in default.";
              type = types.str;
              default = "";
            };
          };
        };

        extraResources =
          cfg:
          let
            mkDeployment = mkBackendDeployment cfg;
          in
          {
            deployments = {
              "${name}-backend" = mkDeployment {
                suffix = "backend";
                command = [
                  "sh"
                  "-c"
                  "alembic upgrade head && uvicorn app.main:app --host 0.0.0.0 --port 8000"
                ];
                ports = [
                  {
                    containerPort = 8000;
                    name = "http";
                    protocol = "TCP";
                  }
                ];
              };

              "${name}-worker" = mkDeployment {
                suffix = "worker";
                command = [
                  "celery"
                  "-A"
                  "app.worker"
                  "worker"
                  "--loglevel=info"
                  "--concurrency=2"
                ];
              };

              "${name}-beat" = mkDeployment {
                suffix = "beat";
                command = [
                  "celery"
                  "-A"
                  "app.worker"
                  "beat"
                  "--loglevel=info"
                ];
              };

              "${name}-frontend" = {
                metadata.labels = {
                  "app.kubernetes.io/instance" = "${name}-frontend";
                  "app.kubernetes.io/name" = "${name}-frontend";
                };

                spec = {
                  replicas = 1;
                  selector.matchLabels = {
                    "app.kubernetes.io/instance" = "${name}-frontend";
                    "app.kubernetes.io/name" = "${name}-frontend";
                  };

                  template = {
                    metadata.labels = {
                      "app.kubernetes.io/instance" = "${name}-frontend";
                      "app.kubernetes.io/name" = "${name}-frontend";
                    };

                    spec = {
                      automountServiceAccountToken = true;
                      serviceAccountName = "default";

                      containers = [
                        {
                          name = "${name}-frontend";
                          image = "ghcr.io/securo-finance/securo-frontend:${cfg.image.tag}";
                          imagePullPolicy = "IfNotPresent";

                          env = [
                            {
                              name = "BACKEND_URL";
                              # Must be fully-qualified: the frontend's nginx
                              # templates this straight into a `resolver`-based
                              # proxy_pass (see its nginx.conf comment about
                              # per-request DNS re-resolution), and nginx's
                              # resolver directive queries the literal name with
                              # no /etc/resolv.conf search-domain expansion.
                              # CoreDNS is only authoritative for the full
                              # "<svc>.<ns>.svc.cluster.local" zone, so the short
                              # "<svc>.<ns>" form that regular ndots-aware tools
                              # (curl, getent) resolve fine comes back NXDOMAIN
                              # from nginx's raw query.
                              value = "http://${name}-backend.${cfg.namespace}.svc.cluster.local:8000";
                            }
                            {
                              name = "FRONTEND_URL";
                              value = "https://${cfg.ingress.domain}";
                            }
                          ];

                          ports = [
                            {
                              containerPort = 8080;
                              name = "http";
                              protocol = "TCP";
                            }
                          ];
                        }
                      ];
                    };
                  };
                };
              };
            }
            // optionalAttrs cfg.agents.enable {
              "${name}-mcp" = mkDeployment {
                suffix = "mcp";
                command = [
                  "uvicorn"
                  "mcp_server.main:app"
                  "--host"
                  "0.0.0.0"
                  "--port"
                  "8765"
                ];
                ports = [
                  {
                    containerPort = 8765;
                    name = "mcp";
                    protocol = "TCP";
                  }
                ];
              };
            };

            services = {
              "${name}-backend".spec = {
                ports = [
                  {
                    name = "http";
                    port = 8000;
                    protocol = "TCP";
                    targetPort = "http";
                  }
                ];
                selector = {
                  "app.kubernetes.io/instance" = "${name}-backend";
                  "app.kubernetes.io/name" = "${name}-backend";
                };
                type = "ClusterIP";
              };

              "${name}-frontend".spec = {
                ports = [
                  {
                    name = "http";
                    port = 8080;
                    protocol = "TCP";
                    targetPort = "http";
                  }
                ];
                selector = {
                  "app.kubernetes.io/instance" = "${name}-frontend";
                  "app.kubernetes.io/name" = "${name}-frontend";
                };
                type = "ClusterIP";
              };
            }
            // optionalAttrs cfg.agents.enable {
              "${name}-mcp".spec = {
                ports = [
                  {
                    name = "mcp";
                    port = 8765;
                    protocol = "TCP";
                    targetPort = "mcp";
                  }
                ];
                selector = {
                  "app.kubernetes.io/instance" = "${name}-mcp";
                  "app.kubernetes.io/name" = "${name}-mcp";
                };
                type = "ClusterIP";
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
                          name = "${name}-frontend";
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
}
