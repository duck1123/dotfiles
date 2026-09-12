{ ... }:
{
  flake.nixidyApps.mealie =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      db-secret = "mealie-database-password";
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
        name = "mealie";
        uses-ingress = true;
        uses-database = true;

        extraOptions = {
          image = mkOption {
            description = mdDoc "The docker image";
            type = types.str;
            default = "ghcr.io/mealie-recipes/mealie:v3.25.1";
          };

          service.port = mkOption {
            description = mdDoc "The service port";
            type = types.int;
            default = 9000;
          };

          replicas = mkOption {
            description = mdDoc "Number of replicas";
            type = types.int;
            default = 1;
          };

        };

        sopsSecrets =
          cfg:
          lib.optionalAttrs (cfg.database.enable && cfg.database.password != "") {
            ${db-secret}.password = cfg.database.password;
          };

        # Shape only -- no volumeHandle here, that's environment-specific (see
        # env/dev/mealie.nix and docs/pinned-volumes.md).
        volumes = cfg: {
          data.size = "5Gi";
        };

        extraResources = cfg: {
          deployments.${name} = {
            metadata.labels = {
              "app.kubernetes.io/instance" = name;
              "app.kubernetes.io/name" = name;
              "app.kubernetes.io/version" = "v3.19.2";
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
                          name = "TZ";
                          value = cfg.tz;
                        }
                        {
                          name = "BASE_URL";
                          value = "https://${cfg.ingress.domain}";
                        }
                      ]
                      ++ lib.optionals cfg.database.enable [
                        {
                          name = "DB_ENGINE";
                          value = "postgres";
                        }
                        {
                          name = "POSTGRES_SERVER";
                          value = cfg.database.host;
                        }
                        {
                          name = "POSTGRES_PORT";
                          value = toString cfg.database.port;
                        }
                        {
                          name = "POSTGRES_DB";
                          value = cfg.database.name;
                        }
                        {
                          name = "POSTGRES_USER";
                          value = cfg.database.username;
                        }
                        {
                          name = "POSTGRES_PASSWORD";
                          valueFrom.secretKeyRef = {
                            name = db-secret;
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
                          path = "/api/app/about";
                          port = cfg.service.port;
                        };
                        initialDelaySeconds = 30;
                        periodSeconds = 10;
                        timeoutSeconds = 5;
                        successThreshold = 1;
                        failureThreshold = 3;
                      };
                      livenessProbe = {
                        httpGet = {
                          path = "/api/app/about";
                          port = cfg.service.port;
                        };
                        initialDelaySeconds = 60;
                        periodSeconds = 30;
                        timeoutSeconds = 5;
                        successThreshold = 1;
                        failureThreshold = 3;
                      };
                      volumeMounts = [
                        {
                          mountPath = "/app/data";
                          name = "data";
                        }
                      ];
                    }
                  ];

                  volumes = [
                    cfg.volumes.data.volume
                  ]
                  ++ lib.optionals (cfg.database.enable && cfg.database.password != "") [
                    {
                      name = db-secret;
                      secret.secretName = db-secret;
                    }
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
