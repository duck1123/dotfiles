{ self, ... }:
{
  flake.nixidyApps.home-assistant =
    {
      config,
      lib,
      self,
      ...
    }:
    with lib;
    self.lib.mkArgoApp { inherit config lib self; } rec {
      name = "home-assistant";
      uses-ingress = true;

      # Shape only -- no volumeHandle here, that's environment-specific (see
      # env/dev/home-assistant.nix and docs/pinned-volumes.md).
      volumes = cfg: {
        config.size = cfg.configSize;
      };

      extraOptions = {
        image = mkOption {
          description = mdDoc "Home Assistant container image (pin a version tag for reproducible deploys)";
          type = types.str;
          default = "ghcr.io/home-assistant/home-assistant:stable";
        };

        timezone = mkOption {
          description = mdDoc "Container TZ (e.g. America/New_York)";
          type = types.str;
          default = "Etc/UTC";
        };

        configSize = mkOption {
          description = mdDoc "PVC size for Home Assistant configuration and state";
          type = types.str;
          default = "10Gi";
        };

        trustedProxyCidrs = mkOption {
          description = mdDoc "CIDRs for reverse-proxy forwarded headers (Kubernetes pod networks are usually in 10.0.0.0/8).";
          type = types.listOf types.str;
          default = [
            "10.0.0.0/8"
            "172.16.0.0/12"
            "192.168.0.0/16"
          ];
        };

        # https://github.com/toxuin/hass-AiDot (community fork — upstream AiDot-Development-Team is unmaintained)
        installAidot = {
          enable = mkEnableOption "Install the AiDot custom integration (hass-AiDot) under /config/custom_components/aidot on each pod start (skips if already at the configured tag)";

          tag = mkOption {
            description = mdDoc "Git tag to fetch from toxuin/hass-AiDot releases (e.g. v1.2.0)";
            type = types.str;
            default = "v1.2.0";
          };
        };
      };

      extraResources = cfg: {
        deployments.${name} = {
          metadata.labels = {
            "app.kubernetes.io/instance" = name;
            "app.kubernetes.io/name" = name;
          };

          spec = {
            replicas = 1;
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

                initContainers = [
                  {
                    name = "ensure-reverse-proxy-config";
                    image = "busybox:1.38";
                    imagePullPolicy = "IfNotPresent";
                    command = [
                      "sh"
                      "-ec"
                      ''
                        CONFIG=/config/configuration.yaml
                        mkdir -p /config
                        CIDRS="${lib.concatStringsSep " " cfg.trustedProxyCidrs}"
                        if [ ! -f "$CONFIG" ]; then
                          {
                            echo "default_config:"
                            echo ""
                            echo "http:"
                            echo "  use_x_forwarded_for: true"
                            echo "  trusted_proxies:"
                          } > "$CONFIG"
                          for cidr in $CIDRS; do
                            echo "    - $cidr" >> "$CONFIG"
                          done
                          exit 0
                        fi
                        if grep -q 'use_x_forwarded_for' "$CONFIG"; then
                          exit 0
                        fi
                        if grep -q '^http:' "$CONFIG"; then
                          echo "home-assistant: configuration.yaml has top-level http: but not use_x_forwarded_for — merge use_x_forwarded_for and trusted_proxies under http: manually" >&2
                          exit 0
                        fi
                        {
                          echo ""
                          echo "http:"
                          echo "  use_x_forwarded_for: true"
                          echo "  trusted_proxies:"
                        } >> "$CONFIG"
                        for cidr in $CIDRS; do
                          echo "    - $cidr" >> "$CONFIG"
                        done
                      ''
                    ];
                    volumeMounts = [
                      {
                        mountPath = "/config";
                        name = "config";
                      }
                    ];
                  }
                ]
                ++ lib.optionals cfg.installAidot.enable [
                  {
                    name = "install-aidot-integration";
                    image = "busybox:1.38";
                    imagePullPolicy = "IfNotPresent";
                    env = [
                      {
                        name = "INSTALL_AIDOT_TAG";
                        value = cfg.installAidot.tag;
                      }
                    ];
                    command = [
                      "sh"
                      "-ec"
                      ''
                        set -e
                        MARKER=/config/custom_components/aidot/.fleetops-aidot-version
                        if [ -f "$MARKER" ] && [ "$(cat "$MARKER")" = "$INSTALL_AIDOT_TAG" ]; then
                          echo "AiDot integration $INSTALL_AIDOT_TAG already installed; skipping"
                          exit 0
                        fi
                        mkdir -p /config/custom_components /tmp/aidot-dl
                        cd /tmp/aidot-dl
                        rm -rf ./*
                        wget -q -O aidot.tgz "https://github.com/toxuin/hass-AiDot/archive/refs/tags/$INSTALL_AIDOT_TAG.tar.gz"
                        tar -xzf aidot.tgz
                        DIR=$(echo hass-AiDot-*)
                        if [ ! -d "$DIR/custom_components/aidot" ]; then
                          echo "hass-AiDot archive missing custom_components/aidot" >&2
                          exit 1
                        fi
                        rm -rf /config/custom_components/aidot
                        cp -r "$DIR/custom_components/aidot" /config/custom_components/
                        printf '%s' "$INSTALL_AIDOT_TAG" > "$MARKER"
                        echo "Installed AiDot integration $INSTALL_AIDOT_TAG"
                      ''
                    ];
                    volumeMounts = [
                      {
                        mountPath = "/config";
                        name = "config";
                      }
                    ];
                  }
                ];

                containers = [
                  {
                    inherit name;
                    image = cfg.image;
                    imagePullPolicy = "IfNotPresent";

                    env = [
                      {
                        name = "TZ";
                        value = cfg.timezone;
                      }
                    ];

                    ports = [
                      {
                        containerPort = 8123;
                        name = "http";
                        protocol = "TCP";
                      }
                    ];

                    livenessProbe = {
                      httpGet = {
                        path = "/";
                        port = "http";
                      };
                      initialDelaySeconds = 120;
                      periodSeconds = 30;
                      timeoutSeconds = 10;
                      failureThreshold = 5;
                    };

                    readinessProbe = {
                      httpGet = {
                        path = "/";
                        port = "http";
                      };
                      initialDelaySeconds = 60;
                      periodSeconds = 15;
                      timeoutSeconds = 10;
                      failureThreshold = 6;
                    };

                    volumeMounts = [
                      {
                        mountPath = "/config";
                        name = "config";
                      }
                    ];
                  }
                ];

                volumes = [ cfg.volumes.config.volume ];
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
              port = 8123;
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
