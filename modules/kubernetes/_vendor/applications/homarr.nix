{ ... }:
{
  flake.nixidyApps.homarr =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      secret-encryption-key-secret = "homarr-secret-encryption-key";
      require-secret-encryption-key =
        cfg:
        assert lib.assertMsg (cfg.secretEncryptionKey != "") ''
          homarr: secretEncryptionKey must be a non-empty string when homarr is enabled.
          Set services.homarr.secretEncryptionKey (e.g. from secrets.homarr.secretEncryptionKey in env).
        '';
        true;
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
        name = "homarr";
        uses-ingress = true;

        sopsSecrets = cfg: {
          ${secret-encryption-key-secret}.SECRET_ENCRYPTION_KEY = cfg.secretEncryptionKey;
        };

        # Shape only -- no volumeHandle here, that's environment-specific (see
        # env/dev/homarr.nix and docs/pinned-volumes.md).
        volumes = cfg: {
          appdata.size = "1Gi";
        };

        extraOptions = {
          image = mkOption {
            description = mdDoc "The Homarr docker image";
            type = types.str;
            default = "ghcr.io/homarr-labs/homarr:latest";
          };

          service.port = mkOption {
            description = mdDoc "The service port";
            type = types.int;
            default = 7575;
          };

          secretEncryptionKey = mkOption {
            description = mdDoc "64-character hex string for encryption (generate with: openssl rand -hex 32). Stored in SOPS secret.";
            type = types.str;
            default = "";
          };

          defaultColorScheme = mkOption {
            description = mdDoc "Default theme: dark or light";
            type = types.str;
            default = "dark";
          };

          disableAnalytics = mkOption {
            description = mdDoc "Disable analytics";
            type = types.bool;
            default = true;
          };

          replicas = mkOption {
            description = mdDoc "Number of replicas";
            type = types.int;
            default = 1;
          };
        };

        extraResources =
          cfg:
          builtins.seq (require-secret-encryption-key cfg) {
            deployments = {
              ${name} = {
                metadata.labels = {
                  "app.kubernetes.io/instance" = name;
                  "app.kubernetes.io/name" = name;
                  "app.kubernetes.io/version" = "latest";
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
                              name = "DEFAULT_COLOR_SCHEME";
                              value = cfg.defaultColorScheme;
                            }
                            {
                              name = "DISABLE_ANALYTICS";
                              value = if cfg.disableAnalytics then "true" else "false";
                            }
                            {
                              name = "SECRET_ENCRYPTION_KEY";
                              valueFrom = {
                                secretKeyRef = {
                                  name = secret-encryption-key-secret;
                                  key = "SECRET_ENCRYPTION_KEY";
                                };
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
                            initialDelaySeconds = 15;
                            periodSeconds = 10;
                            timeoutSeconds = 5;
                            successThreshold = 1;
                            failureThreshold = 3;
                          };
                          livenessProbe = {
                            httpGet = {
                              path = "/";
                              port = cfg.service.port;
                            };
                            initialDelaySeconds = 30;
                            periodSeconds = 30;
                            timeoutSeconds = 5;
                            successThreshold = 1;
                            failureThreshold = 3;
                          };
                          volumeMounts = [
                            {
                              mountPath = "/appdata";
                              name = "appdata";
                            }
                          ];
                        }
                      ];

                      volumes = [ cfg.volumes.appdata.volume ];
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
