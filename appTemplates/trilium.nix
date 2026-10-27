{
  config,
  lib,
  self,
  ...
}:
with lib;
let
  cfg = config.services.trilium;

  # Trilium's "Claude Code" provider doesn't bundle the CLI: it drives
  # whatever binary TRILIUM_CLAUDE_CODE_PATH (or `claude` on PATH) points at,
  # and the triliumnext image ships neither. There's no Dockerfile of ours to
  # add it to, so mount nixpkgs' claude-code via nix-csi. Built as the
  # `trilium-claude-code` flake package (modules/kubernetes/pkgs/) and
  # resolved via the per-system storePath convention, same as
  # appTemplates/duck1123.nix -- see that package's comment for why this
  # isn't a nixExpr string.
  claudeCode = self.packages.x86_64-linux.trilium-claude-code;

  # On the data PVC so the `claude /login` credentials survive pod restarts
  # (HOME itself, /home/node, is part of the image).
  claudeConfigDir = "/home/node/trilium-data/claude";

  claude-secret = "trilium-claude-code";
in
rec {
  name = "trilium";
  uses-ingress = true;
  uses-replicas = true;

  # Shape only -- no volumeHandle here, that's environment-specific (see
  # env/dev/trilium.nix and docs/pinned-volumes.md).
  volumes = cfg: {
    data.size = cfg.dataStorage;
  };

  extraOptions = {
    image = mkOption {
      description = mdDoc "The docker image";
      type = types.str;
      default = "triliumnext/trilium:v0.106.1";
    };

    service.port = mkOption {
      description = mdDoc "The service port";
      type = types.int;
      default = 8080;
    };

    apiKey = mkOption {
      description = mdDoc ''
        Trilium ETAPI token (Options -> ETAPI -> Create new ETAPI token).
        Stored in secrets.enc.yaml as `trilium.key` and wired in via
        `env/dev/trilium.nix`. Only powers the auto-added homepage
        dashboard widget below -- never injected into the trilium
        container itself.
      '';
      type = types.str;
      default = "";
    };

    # Auto-add a Trilium widget to this app's homepage dashboard tile once
    # an ETAPI token is configured -- see appTemplates/immich.nix for the
    # same pattern with more detail. Set
    # `services.homepage.widgetSecrets.TRILIUM_API_KEY` from
    # `config.services.trilium.apiKey` in env/dev/homepage.nix.
    homepage.extraSettings = mkOption {
      default = lib.optionalAttrs (cfg.apiKey != "") {
        widget = {
          type = "trilium";
          url = "http://${name}.${cfg.namespace}:${toString cfg.service.port}";
          key = "{{HOMEPAGE_VAR_TRILIUM_API_KEY}}";
        };
      };
    };

    claudeCode.enable = mkOption {
      description = mdDoc ''
        Mount the Claude Code CLI into the container (via nix-csi) so the
        "Claude Code" AI provider works. Authenticate either by setting
        `claudeCode.oauthToken`, or with a one-time login afterwards:
        `kubectl -n trilium exec -it deploy/trilium -- su node -c /nix/var/result/bin/claude`,
        then `/login`.
      '';
      type = types.bool;
      default = false;
    };

    claudeCode.oauthToken = mkOption {
      description = mdDoc ''
        Long-lived Claude subscription token from `claude setup-token`
        (valid about a year), injected as `CLAUDE_CODE_OAUTH_TOKEN` so no
        login inside the pod is needed. Stored in secrets.enc.yaml as
        `trilium.claudeToken`.
      '';
      type = types.str;
      default = "";
    };

    backendScripting.enable = mkOption {
      description = mdDoc ''
        Allow backend script execution (`TRILIUM_SECURITY_BACKEND_SCRIPTING_ENABLED`).
        Backend scripts have full server access (filesystem, network,
        `child_process`), so only enable if every user is trusted with that.
      '';
      type = types.bool;
      default = false;
    };

    sqlConsole.enable = mkOption {
      description = mdDoc ''
        Enable the SQL console (`TRILIUM_SECURITY_SQL_CONSOLE_ENABLED`), which
        allows raw SQL against the Trilium database.
      '';
      type = types.bool;
      default = false;
    };

    dataStorage = mkOption {
      description = mdDoc "Size of the data PVC";
      type = types.str;
      default = "10Gi";
    };
  };

  sopsSecrets =
    cfg:
    optionalAttrs (cfg.claudeCode.enable && cfg.claudeCode.oauthToken != "") {
      ${claude-secret}.CLAUDE_CODE_OAUTH_TOKEN = cfg.claudeCode.oauthToken;
    };

  extraResources = cfg: {
    deployments.${name} = {
      metadata.labels = {
        "app.kubernetes.io/instance" = name;
        "app.kubernetes.io/name" = name;
      };

      spec = {
        inherit (cfg) replicas;
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
            # Kubernetes injects <SERVICE_NAME>_PORT env vars into every pod; since the
            # Service is named "trilium" this collides with Trilium's own TRILIUM_PORT
            # env var (which expects a plain int, not the injected tcp:// URI).
            enableServiceLinks = false;

            containers = [
              {
                inherit name;
                inherit (cfg) image;
                imagePullPolicy = "IfNotPresent";
                env = [
                  {
                    name = "TZ";
                    value = cfg.tz;
                  }
                  # Always set, so the env var (which wins over config.ini on
                  # the data PVC) is the single source of truth.
                  {
                    name = "TRILIUM_SECURITY_BACKEND_SCRIPTING_ENABLED";
                    value = boolToString cfg.backendScripting.enable;
                  }
                  {
                    name = "TRILIUM_SECURITY_SQL_CONSOLE_ENABLED";
                    value = boolToString cfg.sqlConsole.enable;
                  }
                ]
                ++ optionals cfg.claudeCode.enable [
                  {
                    # Explicit path rather than PATH: start-docker.sh drops to
                    # the node user through `su`, which may reset PATH.
                    name = "TRILIUM_CLAUDE_CODE_PATH";
                    value = "/nix/var/result/bin/claude";
                  }
                  {
                    name = "CLAUDE_CONFIG_DIR";
                    value = claudeConfigDir;
                  }
                ]
                ++ optionals (cfg.claudeCode.enable && cfg.claudeCode.oauthToken != "") [
                  {
                    name = "CLAUDE_CODE_OAUTH_TOKEN";
                    valueFrom.secretKeyRef = {
                      name = claude-secret;
                      key = "CLAUDE_CODE_OAUTH_TOKEN";
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
                  tcpSocket.port = cfg.service.port;
                  initialDelaySeconds = 15;
                  periodSeconds = 10;
                  timeoutSeconds = 5;
                  successThreshold = 1;
                  failureThreshold = 3;
                };
                livenessProbe = {
                  tcpSocket.port = cfg.service.port;
                  initialDelaySeconds = 30;
                  periodSeconds = 30;
                  timeoutSeconds = 5;
                  successThreshold = 1;
                  failureThreshold = 3;
                };
                volumeMounts = [
                  {
                    mountPath = "/home/node/trilium-data";
                    name = "data";
                  }
                ]
                ++ optionals cfg.claudeCode.enable [
                  {
                    mountPath = "/nix";
                    name = "nix";
                    subPath = "nix";
                  }
                ];
              }
            ];

            volumes = [
              cfg.volumes.data.volume
            ]
            ++ optionals cfg.claudeCode.enable [
              {
                name = "nix";
                csi = {
                  driver = "nixkube";
                  volumeAttributes."x86_64-linux" = "${claudeCode}";
                };
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
}
