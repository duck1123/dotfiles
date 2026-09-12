{ ... }:
{
  flake.nixidyApps.paperless-ngx =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      name = "paperless-ngx";
      db-secret = "paperless-ngx-database";
      redis-secret = "paperless-ngx-redis";
      secret-key-secret = "paperless-ngx-secret-key";
      admin-secret = "paperless-ngx-admin";
      enc = pkgs.lib.escapeURL;
      redisDsn =
        cfg:
        "redis://:${enc cfg.redis.password}@${cfg.redis.host}:${toString cfg.redis.port}/${toString cfg.redis.dbIndex}";
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

        # `pvcName` overrides since these predate the "${name}-${name}-<key>"
        # convention. Shape only -- no volumeHandle here, that's
        # environment-specific (see env/dev/paperless-ngx.nix and
        # docs/pinned-volumes.md).
        volumes = cfg: {
          data = {
            pvcName = "${name}-data";
            size = "1Gi";
          };
          media = {
            pvcName = "${name}-media";
            size = "20Gi";
          };
          export = {
            pvcName = "${name}-export";
            size = "1Gi";
          };
          consume = {
            pvcName = "${name}-consume";
            size = "2Gi";
          };
        };

        extraOptions = {
          image = mkOption {
            description = mdDoc "The docker image";
            type = types.str;
            default = "ghcr.io/paperless-ngx/paperless-ngx:3.1.3";
          };

          service.port = mkOption {
            description = mdDoc "The service port";
            type = types.int;
            default = 8000;
          };

          replicas = mkOption {
            description = mdDoc "Number of replicas";
            type = types.int;
            default = 1;
          };

          ocrLanguage = mkOption {
            description = mdDoc "Tesseract OCR language (PAPERLESS_OCR_LANGUAGE)";
            type = types.str;
            default = "eng";
          };

          secretKey = mkOption {
            description = mdDoc "Value for PAPERLESS_SECRET_KEY. When non-empty, a SopsSecret is created and referenced instead of letting the app generate an ephemeral key (which invalidates sessions on every restart).";
            type = types.str;
            default = "";
          };

          adminUser = mkOption {
            description = mdDoc "Initial superuser username (PAPERLESS_ADMIN_USER). Only applied on first run; requires adminPassword to also be set.";
            type = types.str;
            default = "";
          };

          adminPassword = mkOption {
            description = mdDoc "Initial superuser password (PAPERLESS_ADMIN_PASSWORD). Only applied on first run; requires adminUser to also be set.";
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
        };

        sopsSecrets =
          cfg:
          optionalAttrs (cfg.database.password != "") {
            ${db-secret}.password = cfg.database.password;
          }
          // optionalAttrs (cfg.redis.password != "") {
            ${redis-secret}.url = redisDsn cfg;
          }
          // optionalAttrs (cfg.secretKey != "") {
            ${secret-key-secret}.key = cfg.secretKey;
          }
          // optionalAttrs (cfg.adminUser != "" && cfg.adminPassword != "") {
            ${admin-secret} = {
              username = cfg.adminUser;
              password = cfg.adminPassword;
            };
          };

        extraResources =
          cfg: with cfg; {
            deployments.${name} = {
              metadata.labels = {
                "app.kubernetes.io/instance" = name;
                "app.kubernetes.io/name" = name;
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

                    containers = [
                      {
                        inherit name;
                        image = cfg.image;
                        imagePullPolicy = "IfNotPresent";

                        env = [
                          {
                            name = "PAPERLESS_TIME_ZONE";
                            value = cfg.tz;
                          }
                          {
                            name = "PAPERLESS_PORT";
                            value = toString cfg.service.port;
                          }
                          {
                            name = "PAPERLESS_URL";
                            value = "https://${cfg.ingress.domain}";
                          }
                          {
                            name = "PAPERLESS_OCR_LANGUAGE";
                            value = cfg.ocrLanguage;
                          }
                          {
                            name = "PAPERLESS_DBENGINE";
                            value = "postgresql";
                          }
                          {
                            name = "PAPERLESS_DBHOST";
                            value = cfg.database.host;
                          }
                          {
                            name = "PAPERLESS_DBPORT";
                            value = toString cfg.database.port;
                          }
                          {
                            name = "PAPERLESS_DBNAME";
                            value = cfg.database.name;
                          }
                          {
                            name = "PAPERLESS_DBUSER";
                            value = cfg.database.username;
                          }
                        ]
                        ++ optional (cfg.database.password != "") {
                          name = "PAPERLESS_DBPASS";
                          valueFrom.secretKeyRef = {
                            name = db-secret;
                            key = "password";
                          };
                        }
                        ++ (
                          if cfg.redis.password != "" then
                            [
                              {
                                name = "PAPERLESS_REDIS";
                                valueFrom.secretKeyRef = {
                                  name = redis-secret;
                                  key = "url";
                                };
                              }
                            ]
                          else
                            [
                              {
                                name = "PAPERLESS_REDIS";
                                value = "redis://${cfg.redis.host}:${toString cfg.redis.port}/${toString cfg.redis.dbIndex}";
                              }
                            ]
                        )
                        ++ optional (cfg.secretKey != "") {
                          name = "PAPERLESS_SECRET_KEY";
                          valueFrom.secretKeyRef = {
                            name = secret-key-secret;
                            key = "key";
                          };
                        }
                        ++ optionals (cfg.adminUser != "" && cfg.adminPassword != "") [
                          {
                            name = "PAPERLESS_ADMIN_USER";
                            valueFrom.secretKeyRef = {
                              name = admin-secret;
                              key = "username";
                            };
                          }
                          {
                            name = "PAPERLESS_ADMIN_PASSWORD";
                            valueFrom.secretKeyRef = {
                              name = admin-secret;
                              key = "password";
                            };
                          }
                        ];

                        ports = [
                          {
                            containerPort = cfg.service.port;
                            name = "http";
                            protocol = "TCP";
                          }
                        ];

                        readinessProbe = {
                          httpGet = {
                            path = "/";
                            port = cfg.service.port;
                          };
                          initialDelaySeconds = 30;
                          periodSeconds = 15;
                          timeoutSeconds = 10;
                          successThreshold = 1;
                          failureThreshold = 10;
                        };

                        livenessProbe = {
                          httpGet = {
                            path = "/";
                            port = cfg.service.port;
                          };
                          initialDelaySeconds = 60;
                          periodSeconds = 30;
                          timeoutSeconds = 10;
                          successThreshold = 1;
                          failureThreshold = 5;
                        };

                        volumeMounts = [
                          {
                            mountPath = "/usr/src/paperless/data";
                            name = "data";
                          }
                          {
                            mountPath = "/usr/src/paperless/media";
                            name = "media";
                          }
                          {
                            mountPath = "/usr/src/paperless/export";
                            name = "export";
                          }
                          {
                            mountPath = "/usr/src/paperless/consume";
                            name = "consume";
                          }
                        ];
                      }
                    ];

                    volumes = [
                      cfg.volumes.data.volume
                      cfg.volumes.media.volume
                      cfg.volumes.export.volume
                      cfg.volumes.consume.volume
                    ];
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
                    secretName = "${name}-tls";
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
