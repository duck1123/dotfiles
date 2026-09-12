{ ... }:
{
  flake.nixidyApps.hivemq =
    {
      config,
      lib,
      self,
      ...
    }:
    with lib;
    let
      name = "hivemq";
    in
    self.lib.mkArgoApp { inherit config lib self; } rec {
      inherit name;
      uses-ingress = false;

      extraOptions = {
        image = mkOption {
          description = mdDoc "HiveMQ Community Edition image (e.g. hivemq/hivemq-ce:2025.5)";
          type = types.str;
          default = "hivemq/hivemq-ce:2026.5";
        };

        mqttPort = mkOption {
          description = mdDoc "MQTT TCP port (container and Service)";
          type = types.int;
          default = 1883;
        };

        storageClassName = mkOption {
          description = mdDoc "Storage class for broker data (`/opt/hivemq/data`)";
          type = types.str;
          default = "longhorn";
        };

        dataVolumeSize = mkOption {
          description = mdDoc "PVC size for HiveMQ data";
          type = types.str;
          default = "10Gi";
        };

        serviceType = mkOption {
          description = mdDoc "Service type: ClusterIP (in-cluster only) or LoadBalancer (MetalLB VIP for LAN MQTT clients)";
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
          description = mdDoc "Replica count (keep 1 with ReadWriteOnce PVC)";
          type = types.int;
          default = 1;
        };

        useProbes = mkOption {
          description = mdDoc "TCP probes on the MQTT port";
          type = types.bool;
          default = true;
        };
      };

      extraResources = svc: {
        deployments.${name} = {
          metadata.labels = {
            "app.kubernetes.io/instance" = name;
            "app.kubernetes.io/name" = name;
            "app.kubernetes.io/version" = "2025.5";
          };

          spec = {
            replicas = svc.replicas;
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
                securityContext.fsGroup = 10000;

                containers = [
                  {
                    inherit name;
                    image = svc.image;
                    imagePullPolicy = "IfNotPresent";

                    ports = [
                      {
                        containerPort = svc.mqttPort;
                        name = "mqtt";
                        protocol = "TCP";
                      }
                    ];

                    volumeMounts = [
                      {
                        name = "data";
                        mountPath = "/opt/hivemq/data";
                      }
                    ];

                    readinessProbe = mkIf svc.useProbes {
                      tcpSocket = {
                        port = "mqtt";
                      };
                      initialDelaySeconds = 30;
                      periodSeconds = 10;
                      timeoutSeconds = 5;
                      successThreshold = 1;
                      failureThreshold = 6;
                    };

                    livenessProbe = mkIf svc.useProbes {
                      tcpSocket = {
                        port = "mqtt";
                      };
                      initialDelaySeconds = 90;
                      periodSeconds = 30;
                      timeoutSeconds = 5;
                      successThreshold = 1;
                      failureThreshold = 5;
                    };

                    resources = {
                      requests = {
                        cpu = "250m";
                        memory = "512Mi";
                      };
                      limits = {
                        cpu = "2";
                        memory = "2Gi";
                      };
                    };
                  }
                ];

                volumes = [
                  {
                    name = "data";
                    persistentVolumeClaim.claimName = "${name}-${name}-data";
                  }
                ];
              };
            };
          };
        };

        services.${name}.spec = {
          ports = [
            {
              name = "mqtt";
              port = svc.mqttPort;
              protocol = "TCP";
              targetPort = "mqtt";
            }
          ];

          selector = {
            "app.kubernetes.io/instance" = name;
            "app.kubernetes.io/name" = name;
          };

          type = svc.serviceType;
        }
        // optionalAttrs (svc.loadBalancerIP != null) {
          loadBalancerIP = svc.loadBalancerIP;
        };

        persistentVolumeClaims."${name}-${name}-data".spec = {
          inherit (svc) storageClassName;
          accessModes = [ "ReadWriteOnce" ];
          resources.requests.storage = svc.dataVolumeSize;
        };
      };
    };
}
