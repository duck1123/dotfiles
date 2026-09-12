{ ... }:
{
  flake.nixidyApps.crafty-controller =
    {
      config,
      lib,
      self,
      ...
    }:
    with lib;
    let
      name = "crafty-controller";
    in
    self.lib.mkArgoApp { inherit config lib self; } rec {
      inherit name;
      # Panel is HTTPS-only (self-signed) and Minecraft server ports are raw
      # TCP/UDP -- neither fits this repo's HTTP-ingress model, so this app
      # is exposed directly via a MetalLB LoadBalancer Service instead (same
      # pattern as applications/hivemq.nix).
      uses-ingress = false;

      # Shape only -- no volumeHandle here, that's environment-specific (see
      # env/dev/crafty-controller.nix and docs/pinned-volumes.md).
      volumes = cfg: {
        config.size = "1Gi";
        backups.size = "10Gi";
        logs.size = "2Gi";
        servers.size = cfg.serversStorage;
        import.size = "5Gi";
      };

      extraOptions = {
        image = mkOption {
          description = mdDoc "The docker image";
          type = types.str;
          default = "registry.gitlab.com/crafty-controller/crafty-4:4.5.4";
        };

        serversStorage = mkOption {
          description = mdDoc "Size of the servers PVC (holds all managed Minecraft server worlds/jars)";
          type = types.str;
          default = "20Gi";
        };

        panelPort = mkOption {
          description = mdDoc "Crafty web panel port (HTTPS, self-signed by Crafty itself)";
          type = types.port;
          default = 8443;
        };

        dynmapPort = mkOption {
          description = mdDoc "Dynmap port, only used if a managed server runs the Dynmap plugin";
          type = types.port;
          default = 8123;
        };

        serverPortRange = {
          from = mkOption {
            description = mdDoc "First TCP port in the range handed out to managed Minecraft servers";
            type = types.port;
            default = 25565;
          };

          to = mkOption {
            description = mdDoc "Last TCP port (inclusive) in the range handed out to managed Minecraft servers. Widen this in env/dev/crafty-controller.nix if more concurrent servers are needed.";
            type = types.port;
            default = 25570;
          };
        };

        enableBedrock = mkOption {
          description = mdDoc "Expose the UDP port used by Bedrock Minecraft servers";
          type = types.bool;
          default = false;
        };

        bedrockPort = mkOption {
          description = mdDoc "Bedrock Minecraft server UDP port";
          type = types.port;
          default = 19132;
        };

        serviceType = mkOption {
          description = mdDoc "Service type: ClusterIP (in-cluster only) or LoadBalancer (MetalLB VIP for LAN access to the panel and managed servers)";
          type = types.enum [
            "ClusterIP"
            "LoadBalancer"
          ];
          default = "LoadBalancer";
        };

        loadBalancerIP = mkOption {
          description = mdDoc "Optional fixed MetalLB IP when `serviceType` is LoadBalancer";
          type = types.nullOr types.str;
          default = null;
        };

        replicas = mkOption {
          description = mdDoc "Number of replicas";
          type = types.int;
          default = 1;
        };

        useProbes = mkOption {
          description = mdDoc "Enable readiness and liveness probes against the panel port";
          type = types.bool;
          default = true;
        };
      };

      extraResources =
        cfg:
        let
          mcPorts = lib.range cfg.serverPortRange.from cfg.serverPortRange.to;
          mcContainerPorts = map (port: {
            containerPort = port;
            name = "mc-${toString port}";
            protocol = "TCP";
          }) mcPorts;
          mcServicePorts = map (port: {
            name = "mc-${toString port}";
            port = port;
            protocol = "TCP";
            targetPort = "mc-${toString port}";
          }) mcPorts;
        in
        {
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
                          name = "TZ";
                          value = cfg.tz;
                        }
                      ];

                      ports = [
                        {
                          containerPort = cfg.panelPort;
                          name = "panel";
                          protocol = "TCP";
                        }
                        {
                          containerPort = cfg.dynmapPort;
                          name = "dynmap";
                          protocol = "TCP";
                        }
                      ]
                      ++ mcContainerPorts
                      ++ lib.optionals cfg.enableBedrock [
                        {
                          containerPort = cfg.bedrockPort;
                          name = "bedrock";
                          protocol = "UDP";
                        }
                      ];

                      readinessProbe = lib.mkIf cfg.useProbes {
                        tcpSocket.port = "panel";
                        initialDelaySeconds = 30;
                        periodSeconds = 10;
                        timeoutSeconds = 5;
                        successThreshold = 1;
                        failureThreshold = 6;
                      };
                      livenessProbe = lib.mkIf cfg.useProbes {
                        tcpSocket.port = "panel";
                        initialDelaySeconds = 60;
                        periodSeconds = 30;
                        timeoutSeconds = 5;
                        successThreshold = 1;
                        failureThreshold = 5;
                      };

                      resources = {
                        requests = {
                          cpu = "500m";
                          memory = "1Gi";
                        };
                        limits = {
                          cpu = "4";
                          memory = "8Gi";
                        };
                      };

                      volumeMounts = [
                        {
                          name = "config";
                          mountPath = "/crafty/app/config";
                        }
                        {
                          name = "backups";
                          mountPath = "/crafty/backups";
                        }
                        {
                          name = "logs";
                          mountPath = "/crafty/logs";
                        }
                        {
                          name = "servers";
                          mountPath = "/crafty/servers";
                        }
                        {
                          name = "import";
                          mountPath = "/crafty/import";
                        }
                      ];
                    }
                  ];

                  volumes = [
                    cfg.volumes.config.volume
                    cfg.volumes.backups.volume
                    cfg.volumes.logs.volume
                    cfg.volumes.servers.volume
                    cfg.volumes.import.volume
                  ];
                };
              };
            };
          };

          services.${name}.spec =
            {
              ports = [
                {
                  name = "panel";
                  port = cfg.panelPort;
                  protocol = "TCP";
                  targetPort = "panel";
                }
                {
                  name = "dynmap";
                  port = cfg.dynmapPort;
                  protocol = "TCP";
                  targetPort = "dynmap";
                }
              ]
              ++ mcServicePorts
              ++ lib.optionals cfg.enableBedrock [
                {
                  name = "bedrock";
                  port = cfg.bedrockPort;
                  protocol = "UDP";
                  targetPort = "bedrock";
                }
              ];

              selector = {
                "app.kubernetes.io/instance" = name;
                "app.kubernetes.io/name" = name;
              };

              type = cfg.serviceType;
            }
            // optionalAttrs (cfg.loadBalancerIP != null) {
              loadBalancerIP = cfg.loadBalancerIP;
            };
        };
    };
}
