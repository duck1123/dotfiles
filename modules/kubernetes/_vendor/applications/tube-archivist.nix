{ ... }:
{
  flake.nixidyApps.tube-archivist =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      auth-secret = "tube-archivist-auth";
      elastic-secret = "tube-archivist-elastic-password";
      redis-con-secret = "tube-archivist-redis-con";
      # Minimal URL-encoding for Redis password in connection string
      urlEnc =
        s:
        builtins.replaceStrings
          [
            "%"
            "@"
            ":"
            "/"
            "?"
            "#"
            "["
            "]"
          ]
          [
            "%25"
            "%40"
            "%3A"
            "%2F"
            "%3F"
            "%23"
            "%5B"
            "%5D"
          ]
          s;
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
        name = "tube-archivist";
        uses-ingress = true;

        sopsSecrets =
          cfg:
          (lib.optionalAttrs (cfg.auth.username != "" && cfg.auth.password != "") {
            ${auth-secret} = {
              username = cfg.auth.username;
              password = cfg.auth.password;
            };
          })
          // (lib.optionalAttrs (cfg.elasticsearch.elasticPassword != "") {
            ${elastic-secret} = {
              password = cfg.elasticsearch.elasticPassword;
            };
          })
          // (lib.optionalAttrs (cfg.redis.password != "") {
            ${redis-con-secret} = {
              connectionString = "redis://:${urlEnc cfg.redis.password}@${cfg.redis.host}:${toString cfg.redis.port}";
            };
          });

        extraOptions = {
          image = mkOption {
            description = mdDoc "The Tube Archivist docker image";
            type = types.str;
            default = "bbilly1/tubearchivist:latest";
          };

          service.port = mkOption {
            description = mdDoc "The service port";
            type = types.int;
            default = 8000;
          };

          nfs = {
            enable = mkOption {
              description = mdDoc "Enable NFS for YouTube library and cache volumes";
              type = types.bool;
              default = true;
            };

            server = mkOption {
              description = mdDoc "NFS server hostname/IP";
              type = types.str;
              default = "nasnix";
            };

            path = mkOption {
              description = mdDoc "Base NFS server path; /Youtube and /YT-Cache are appended under this path";
              type = types.str;
              default = "/mnt/media";
            };
          };

          pgid = mkOption {
            description = mdDoc "The group ID";
            type = types.int;
            default = 1000;
          };

          puid = mkOption {
            description = mdDoc "The user ID";
            type = types.int;
            default = 1000;
          };

          replicas = mkOption {
            description = mdDoc "Number of Tube Archivist replicas";
            type = types.int;
            default = 1;
          };

          esUrl = mkOption {
            description = mdDoc "Elasticsearch URL (ES_URL) used by Tube Archivist";
            type = types.str;
            default = "http://tube-archivist-es:9200";
          };

          redis = {
            host = mkOption {
              description = mdDoc "Redis host for REDIS_CON connection string";
              type = types.str;
              default = "redis.redis";
            };

            port = mkOption {
              description = mdDoc "Redis port for REDIS_CON connection string";
              type = types.int;
              default = 6379;
            };

            password = mkOption {
              description = mdDoc "Redis password. If set, REDIS_CON is stored in a secret (redis://:password@host:port).";
              type = types.str;
              default = "";
            };
          };

          auth = {
            username = mkOption {
              description = mdDoc "Initial Tube Archivist username (TA_USERNAME). If empty, container defaults are used.";
              type = types.str;
              default = "";
            };

            password = mkOption {
              description = mdDoc "Initial Tube Archivist password (TA_PASSWORD). If empty, container defaults are used.";
              type = types.str;
              default = "";
            };
          };

          elasticsearch = {
            enable = mkOption {
              description = mdDoc "Deploy bundled Elasticsearch instance (bbilly1/tubearchivist-es).";
              type = types.bool;
              default = true;
            };

            storageClassName = mkOption {
              description = mdDoc "Storage class for Elasticsearch data volume";
              type = types.str;
              default = "longhorn";
            };

            elasticPassword = mkOption {
              description = mdDoc "Elasticsearch password (ELASTIC_PASSWORD). If empty, password auth is not configured.";
              type = types.str;
              default = "";
            };

            javaOpts = mkOption {
              description = mdDoc "ES_JAVA_OPTS for Elasticsearch container";
              type = types.str;
              default = "-Xms512m -Xmx512m";
            };
          };
        };

        extraResources = cfg: {
          deployments = {
            ${name} = {
              metadata.labels = {
                "app.kubernetes.io/instance" = name;
                "app.kubernetes.io/name" = name;
                "app.kubernetes.io/version" = "latest";
              };

              spec = {
                replicas = cfg.replicas;
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
                            name = "PGID";
                            value = "${toString cfg.pgid}";
                          }
                          {
                            name = "PUID";
                            value = "${toString cfg.puid}";
                          }
                          {
                            name = "TZ";
                            value = cfg.tz;
                          }
                          {
                            name = "TA_HOST";
                            value = cfg.ingress.domain;
                          }
                          {
                            name = "ES_URL";
                            value = cfg.esUrl;
                          }
                          (
                            if cfg.redis.password != "" then
                              {
                                name = "REDIS_CON";
                                valueFrom = {
                                  secretKeyRef = {
                                    name = redis-con-secret;
                                    key = "connectionString";
                                  };
                                };
                              }
                            else
                              {
                                name = "REDIS_CON";
                                value = "redis://${cfg.redis.host}:${toString cfg.redis.port}";
                              }
                          )
                        ]
                        ++ (lib.optionals (cfg.auth.username != "" && cfg.auth.password != "") [
                          {
                            name = "TA_USERNAME";
                            valueFrom.secretKeyRef = {
                              name = auth-secret;
                              key = "username";
                            };
                          }
                          {
                            name = "TA_PASSWORD";
                            valueFrom.secretKeyRef = {
                              name = auth-secret;
                              key = "password";
                            };
                          }
                        ])
                        ++ (lib.optionals (cfg.elasticsearch.elasticPassword != "") [
                          {
                            name = "ELASTIC_PASSWORD";
                            valueFrom.secretKeyRef = {
                              name = elastic-secret;
                              key = "password";
                            };
                          }
                        ]);
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
                          initialDelaySeconds = 60;
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
                          initialDelaySeconds = 90;
                          periodSeconds = 30;
                          timeoutSeconds = 5;
                          successThreshold = 1;
                          failureThreshold = 3;
                        };
                        volumeMounts = [
                          {
                            mountPath = "/config";
                            name = "config";
                          }
                          {
                            mountPath = "/youtube";
                            name = "youtube";
                          }
                          {
                            mountPath = "/cache";
                            name = "cache";
                          }
                        ];
                      }
                    ];

                    volumes = [
                      {
                        name = "config";
                        persistentVolumeClaim.claimName = "${name}-${name}-config";
                      }
                      {
                        name = "youtube";
                        persistentVolumeClaim.claimName = "${name}-${name}-youtube";
                      }
                      {
                        name = "cache";
                        persistentVolumeClaim.claimName = "${name}-${name}-cache";
                      }
                    ];
                  };
                };
              };
            };
          }
          // lib.optionalAttrs cfg.elasticsearch.enable {
            "${name}-es" = {
              metadata.labels = {
                "app.kubernetes.io/instance" = "${name}-es";
                "app.kubernetes.io/name" = "${name}-es";
                "app.kubernetes.io/version" = "latest";
              };

              spec = {
                replicas = 1;
                selector.matchLabels = {
                  "app.kubernetes.io/instance" = "${name}-es";
                  "app.kubernetes.io/name" = "${name}-es";
                };

                template = {
                  metadata.labels = {
                    "app.kubernetes.io/instance" = "${name}-es";
                    "app.kubernetes.io/name" = "${name}-es";
                  };

                  spec = {
                    automountServiceAccountToken = true;
                    serviceAccountName = "default";

                    # Elasticsearch runs as UID 1000; PVC is often root-owned. Chown so ES can write node.lock.
                    initContainers = [
                      {
                        name = "fix-es-data-permissions";
                        image = "busybox:latest";
                        imagePullPolicy = "IfNotPresent";
                        command = [
                          "sh"
                          "-c"
                          "chown -R 1000:1000 /usr/share/elasticsearch/data"
                        ];
                        securityContext.runAsUser = 0;
                        volumeMounts = [
                          {
                            mountPath = "/usr/share/elasticsearch/data";
                            name = "es-data";
                          }
                        ];
                      }
                    ];

                    containers = [
                      {
                        name = "${name}-es";
                        image = "bbilly1/tubearchivist-es:latest";
                        imagePullPolicy = "IfNotPresent";
                        env = [
                          {
                            name = "ES_JAVA_OPTS";
                            value = cfg.elasticsearch.javaOpts;
                          }
                          # Single-node discovery (required when binding to non-loopback)
                          {
                            name = "discovery.type";
                            value = "single-node";
                          }
                          # Disable security so transport SSL is not required (internal single-node)
                          {
                            name = "xpack.security.enabled";
                            value = "false";
                          }
                          # Snapshot repo path (required by Tube Archivist ES checks)
                          {
                            name = "path.repo";
                            value = "/usr/share/elasticsearch/data/snapshot";
                          }
                        ]
                        ++ (lib.optionals (cfg.elasticsearch.elasticPassword != "") [
                          {
                            name = "ELASTIC_PASSWORD";
                            valueFrom.secretKeyRef = {
                              name = elastic-secret;
                              key = "password";
                            };
                          }
                        ]);
                        ports = [
                          {
                            containerPort = 9200;
                            name = "http";
                            protocol = "TCP";
                          }
                        ];
                        volumeMounts = [
                          {
                            mountPath = "/usr/share/elasticsearch/data";
                            name = "es-data";
                          }
                        ];
                      }
                    ];

                    volumes = [
                      {
                        name = "es-data";
                        persistentVolumeClaim.claimName = "${name}-es-data";
                      }
                    ];
                  };
                };
              };
            };
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

            tls = [ { hosts = [ domain ]; } ];
          };

          persistentVolumeClaims = {
            "${name}-${name}-config".spec = {
              inherit (cfg) storageClassName;
              accessModes = [ "ReadWriteOnce" ];
              resources.requests.storage = "5Gi";
            };
            "${name}-${name}-youtube".spec =
              if cfg.nfs.enable then
                {
                  accessModes = [ "ReadWriteMany" ];
                  resources.requests.storage = "1Gi";
                  storageClassName = "";
                  volumeName = "${name}-${name}-youtube-nfs";
                }
              else
                {
                  inherit (cfg) storageClassName;
                  accessModes = [ "ReadWriteOnce" ];
                  resources.requests.storage = "200Gi";
                };
            "${name}-${name}-cache".spec =
              if cfg.nfs.enable then
                {
                  accessModes = [ "ReadWriteMany" ];
                  resources.requests.storage = "1Gi";
                  storageClassName = "";
                  volumeName = "${name}-${name}-cache-nfs";
                }
              else
                {
                  inherit (cfg) storageClassName;
                  accessModes = [ "ReadWriteOnce" ];
                  resources.requests.storage = "50Gi";
                };
          }
          // lib.optionalAttrs cfg.elasticsearch.enable {
            "${name}-es-data".spec = {
              storageClassName = cfg.elasticsearch.storageClassName;
              accessModes = [ "ReadWriteOnce" ];
              resources.requests.storage = "50Gi";
            };
          };

          services = {
            ${name}.spec = {
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
          }
          // lib.optionalAttrs cfg.elasticsearch.enable {
            "${name}-es".spec = {
              ports = [
                {
                  name = "http";
                  port = 9200;
                  protocol = "TCP";
                  targetPort = "http";
                }
              ];

              selector = {
                "app.kubernetes.io/instance" = "${name}-es";
                "app.kubernetes.io/name" = "${name}-es";
              };

              type = "ClusterIP";
            };
          };

          # Create NFS PersistentVolumes for YouTube library and cache when NFS is enabled
          persistentVolumes = lib.optionalAttrs cfg.nfs.enable {
            "${name}-${name}-youtube-nfs" = {
              apiVersion = "v1";
              kind = "PersistentVolume";
              metadata = {
                name = "${name}-${name}-youtube-nfs";
              };
              spec = {
                capacity = {
                  storage = "2Ti";
                };
                accessModes = [ "ReadWriteMany" ];
                mountOptions = [
                  "nolock"
                  "noexec"
                  "soft"
                  "timeo=30"
                ];
                nfs = {
                  server = cfg.nfs.server;
                  path = "${cfg.nfs.path}/Youtube";
                };
                persistentVolumeReclaimPolicy = "Retain";
              };
            };
            "${name}-${name}-cache-nfs" = {
              apiVersion = "v1";
              kind = "PersistentVolume";
              metadata = {
                name = "${name}-${name}-cache-nfs";
              };
              spec = {
                capacity = {
                  storage = "500Gi";
                };
                accessModes = [ "ReadWriteMany" ];
                mountOptions = [
                  "nolock"
                  "noexec"
                  "soft"
                  "timeo=30"
                ];
                nfs = {
                  server = cfg.nfs.server;
                  path = "${cfg.nfs.path}/YT-Cache";
                };
                persistentVolumeReclaimPolicy = "Retain";
              };
            };
          };
        };
      };
}
