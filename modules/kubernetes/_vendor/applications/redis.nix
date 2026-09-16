_: {
  flake.nixidyApps.redis =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      password-secret = "redis-password";
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
        name = "redis";

        sopsSecrets = cfg: {
          ${password-secret} = {
            inherit (cfg) password;
          };
        };

        # Shape only -- no volumeHandle here, that's environment-specific (see
        # docs/pinned-volumes.md).
        volumes = cfg: {
          data.size = "10Gi";
        };

        extraOptions = {
          image = mkOption {
            description = mdDoc "The docker image";
            type = types.str;
            default = "redis:8-alpine";
          };

          password = mkOption {
            description = mdDoc "The password";
            type = types.str;
            default = "CHANGEME";
          };

          port = mkOption {
            description = mdDoc "The Redis port";
            type = types.int;
            default = 6379;
          };

          replicas = mkOption {
            description = mdDoc "Number of Redis replicas";
            type = types.int;
            default = 1;
          };

          repairAof = mkOption {
            description = mdDoc "Deploy a one-shot job that runs redis-check-aof --fix on all incremental AOF files";
            type = types.bool;
            default = false;
          };
        };

        extraResources =
          cfg:
          lib.throwIf (cfg.repairAof && cfg.replicas > 0)
            "redis: set replicas = 0 before enabling repairAof to avoid concurrent PVC access"
            {
              deployments = {
                redis = {
                  metadata.labels = {
                    "app.kubernetes.io/instance" = name;
                    "app.kubernetes.io/name" = name;
                  };

                  spec = {
                    inherit (cfg) replicas;
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
                            inherit (cfg) image;
                            name = "redis";
                            imagePullPolicy = "IfNotPresent";
                            command = [
                              "sh"
                              "-c"
                              "redis-server --requirepass \"$REDIS_PASSWORD\" --appendonly yes --aof-load-corrupt-tail-max-size 1181"
                            ];
                            env = [
                              {
                                name = "REDIS_PASSWORD";
                                valueFrom.secretKeyRef = {
                                  name = password-secret;
                                  key = "password";
                                };
                              }
                            ];
                            ports = [
                              {
                                containerPort = cfg.port;
                                name = "redis";
                                protocol = "TCP";
                              }
                            ];

                            livenessProbe = {
                              exec = {
                                command = [
                                  "sh"
                                  "-c"
                                  "redis-cli --no-auth-warning -a \"$REDIS_PASSWORD\" ping"
                                ];
                              };
                              initialDelaySeconds = 30;
                              periodSeconds = 10;
                              timeoutSeconds = 5;
                            };

                            readinessProbe = {
                              exec = {
                                command = [
                                  "sh"
                                  "-c"
                                  "redis-cli --no-auth-warning -a \"$REDIS_PASSWORD\" ping"
                                ];
                              };
                              initialDelaySeconds = 5;
                              periodSeconds = 5;
                              timeoutSeconds = 3;
                            };

                            volumeMounts = [
                              {
                                mountPath = "/data";
                                name = "data";
                              }
                            ];
                          }
                        ];
                        volumes = [
                          cfg.volumes.data.volume
                        ];
                      };
                    };
                  };
                };
              };

              services = {
                redis.spec = {
                  ports = [
                    {
                      inherit (cfg) port;
                      name = "redis";
                      protocol = "TCP";
                      targetPort = "redis";
                    }
                  ];

                  selector = {
                    "app.kubernetes.io/instance" = name;
                    "app.kubernetes.io/name" = name;
                  };

                  type = "ClusterIP";
                };
              };

              jobs = lib.optionalAttrs cfg.repairAof {
                "${name}-aof-repair" = {
                  spec = {
                    backoffLimit = 0;
                    ttlSecondsAfterFinished = 300;
                    template.spec = {
                      restartPolicy = "Never";
                      volumes = [
                        cfg.volumes.data.volume
                      ];
                      containers = [
                        {
                          inherit (cfg) image;
                          name = "aof-repair";
                          imagePullPolicy = "IfNotPresent";
                          command = [
                            "sh"
                            "-c"
                            "for f in /data/appendonlydir/*.incr.aof; do echo \"Fixing $f\"; yes | redis-check-aof --fix \"$f\"; done"
                          ];
                          volumeMounts = [
                            {
                              mountPath = "/data";
                              name = "data";
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
