{ ... }:
{
  flake.nixidyApps.attic =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      name = "attic";
      labels = {
        "app.kubernetes.io/name" = name;
        "app.kubernetes.io/instance" = name;
      };

      token-secret = "attic-token-hs256";
      db-secret = "attic-database-url";
      s3-secret = "attic-s3-credentials";
      enc = pkgs.lib.escapeURL;
      databaseUrl =
        cfg:
        "postgresql://${enc cfg.database.username}:${enc cfg.database.password}@${cfg.database.host}:${toString cfg.database.port}/${cfg.database.name}";

      require-token-secret =
        cfg:
        assert lib.assertMsg (cfg.tokenHs256SecretBase64 != "") ''
          attic: tokenHs256SecretBase64 must be a non-empty string when attic is enabled.
          Set services.attic.tokenHs256SecretBase64 (e.g. from secrets.attic.tokenHs256SecretBase64 in env).
          Generate with: openssl rand -base64 32
        '';
        true;

      nixExpr = ''
        (builtins.getFlake "github:duck1123/k3s-fleetops").packages.x86_64-linux.attic-server-bundle
      '';

      serverConfig = cfg: ''
        listen = "[::]:${toString cfg.service.port}"
        allowed-hosts = ["${cfg.ingress.domain}", "localhost:${toString cfg.service.port}", "${name}.${name}:${toString cfg.service.port}"]
        api-endpoint = "https://${cfg.ingress.domain}/"

        # url intentionally omitted: falls back to $ATTIC_SERVER_DATABASE_URL.
        # The [database] header must still be present or serde fails with
        # "missing field `database`" instead of applying the per-field default.
        [database]

        [storage]
        type = "s3"
        region = "${cfg.storage.region}"
        bucket = "${cfg.storage.bucket}"
        endpoint = "${cfg.storage.endpoint}"

        # Upstream defaults from server/src/config-template.toml — these fields
        # have no serde defaults of their own, unlike [database]'s url.
        [chunking]
        nar-size-threshold = 65536
        min-size = 16384
        avg-size = 65536
        max-size = 262144

        [compression]
        type = "zstd"
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
            description = mdDoc "atticd API listen port";
            type = types.int;
            default = 8080;
          };

          tokenHs256SecretBase64 = mkOption {
            description = mdDoc "Base64-encoded HS256 JWT signing secret (generate with: openssl rand -base64 32). Stored in a SOPS secret.";
            type = types.str;
            default = "";
          };

          storage = {
            bucket = mkOption {
              description = mdDoc "S3 bucket name for cache storage";
              type = types.str;
              default = "attic";
            };
            region = mkOption {
              description = mdDoc "S3 region";
              type = types.str;
              default = "us-east-1";
            };
            endpoint = mkOption {
              description = mdDoc "S3-compatible endpoint URL";
              type = types.str;
              default = "http://garage.garage:3900";
            };

            accessKey = mkOption {
              description = mdDoc "S3 access key (the S3 provider lives in a different namespace, so this needs its own copy of the credential rather than referencing its secret cross-namespace)";
              type = types.str;
              default = "";
            };

            secretKey = mkOption {
              description = mdDoc "S3 secret key";
              type = types.str;
              default = "";
            };
          };
        };

        sopsSecrets =
          cfg:
          optionalAttrs (cfg.tokenHs256SecretBase64 != "") {
            ${token-secret}.TOKEN_HS256_SECRET_BASE64 = cfg.tokenHs256SecretBase64;
          }
          // optionalAttrs (cfg.database.password != "") {
            ${db-secret}.DATABASE_URL = databaseUrl cfg;
          }
          // optionalAttrs (cfg.storage.accessKey != "" && cfg.storage.secretKey != "") {
            ${s3-secret} = {
              AWS_ACCESS_KEY_ID = cfg.storage.accessKey;
              AWS_SECRET_ACCESS_KEY = cfg.storage.secretKey;
            };
          };

        extraResources =
          cfg:
          builtins.seq (require-token-secret cfg) {
            configMaps.${name}.data."server.toml" = serverConfig cfg;

            deployments.${name}.spec = {
              replicas = 1;
              selector.matchLabels = labels;
              template = {
                metadata.labels = labels;
                spec = {
                  containers = [
                    {
                      inherit name;
                      image = "ghcr.io/lillecarl/nix-csi/scratch:1.0.1";
                      command = [
                        "atticd"
                        "-f"
                        "/etc/atticd/server.toml"
                      ];
                      env = [
                        {
                          name = "TZ";
                          value = cfg.tz;
                        }
                        {
                          name = "ATTIC_SERVER_TOKEN_HS256_SECRET_BASE64";
                          valueFrom.secretKeyRef = {
                            name = token-secret;
                            key = "TOKEN_HS256_SECRET_BASE64";
                          };
                        }
                        {
                          name = "ATTIC_SERVER_DATABASE_URL";
                          valueFrom.secretKeyRef = {
                            name = db-secret;
                            key = "DATABASE_URL";
                          };
                        }
                        {
                          name = "AWS_ACCESS_KEY_ID";
                          valueFrom.secretKeyRef = {
                            name = s3-secret;
                            key = "AWS_ACCESS_KEY_ID";
                          };
                        }
                        {
                          name = "AWS_SECRET_ACCESS_KEY";
                          valueFrom.secretKeyRef = {
                            name = s3-secret;
                            key = "AWS_SECRET_ACCESS_KEY";
                          };
                        }
                        {
                          name = "SSL_CERT_FILE";
                          value = "/nix/var/result/etc/ssl/certs/ca-bundle.crt";
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
                        tcpSocket.port = cfg.service.port;
                        initialDelaySeconds = 10;
                        periodSeconds = 10;
                        timeoutSeconds = 5;
                        failureThreshold = 6;
                      };
                      livenessProbe = {
                        tcpSocket.port = cfg.service.port;
                        # Allow time for the first nix-csi build (Rust binary fetch/build can be slow)
                        initialDelaySeconds = 120;
                        periodSeconds = 30;
                        timeoutSeconds = 5;
                        failureThreshold = 3;
                      };
                      volumeMounts = [
                        {
                          name = "nix";
                          mountPath = "/nix";
                          subPath = "nix";
                        }
                        {
                          name = "config";
                          mountPath = "/etc/atticd";
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
                    {
                      name = "config";
                      configMap.name = name;
                    }
                  ];
                };
              };
            };

            ingresses.${name} = {
              metadata.annotations."cert-manager.io/cluster-issuer" = cfg.ingress.clusterIssuer;
              spec = with cfg.ingress; {
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
                tls = [
                  {
                    hosts = [ domain ];
                    secretName = "${name}-tls";
                  }
                ];
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
              ];
              type = "ClusterIP";
            };
          };
      };
}
