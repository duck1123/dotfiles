{ ... }:
{
  flake.nixidyApps.dozzle =
    {
      config,
      lib,
      self,
      ...
    }:
    with lib;
    self.lib.mkArgoApp { inherit config lib; } rec {
      name = "dozzle";
      uses-ingress = true;

      extraOptions = {
        image = mkOption {
          description = mdDoc "The docker image";
          type = types.str;
          default = "amir20/dozzle:latest";
        };

        service.port = mkOption {
          description = mdDoc "The service port";
          type = types.int;
          default = 8080;
        };

        noAnalytics = mkOption {
          description = mdDoc "Disable analytics";
          type = types.bool;
          default = true;
        };

        filter = mkOption {
          description = mdDoc "Filter containers by name (comma-separated)";
          type = types.str;
          default = "";
        };

        level = mkOption {
          description = mdDoc "Set the level of logs to show (all, info, warn, error)";
          type = types.str;
          default = "all";
        };

        mountContainerdSocket = mkOption {
          description = mdDoc "Mount containerd socket from host (for k3s). Requires hostPath access.";
          type = types.bool;
          default = true;
        };
      };

      extraResources = cfg: {
        serviceAccounts = {
          ${name} = {
            metadata = {
              name = name;
              namespace = cfg.namespace;
            };
          };
        };

        clusterRoles = {
          ${name} = {
            metadata = {
              name = name;
            };
            rules = [
              {
                apiGroups = [ "" ];
                resources = [
                  "pods"
                  "pods/log"
                  "nodes"
                ];
                verbs = [
                  "get"
                  "list"
                  "watch"
                ];
              }
              {
                apiGroups = [ "" ];
                resources = [ "namespaces" ];
                verbs = [
                  "get"
                  "list"
                ];
              }
              {
                apiGroups = [ "metrics.k8s.io" ];
                resources = [ "pods" ];
                verbs = [
                  "get"
                  "list"
                ];
              }
            ];
          };
        };

        clusterRoleBindings = {
          ${name} = {
            metadata = {
              name = name;
            };
            roleRef = {
              apiGroup = "rbac.authorization.k8s.io";
              kind = "ClusterRole";
              name = name;
            };
            subjects = [
              {
                apiGroup = "";
                kind = "ServiceAccount";
                name = name;
                namespace = cfg.namespace;
              }
            ];
          };
        };

        deployments = {
          ${name} = {
            metadata.labels = {
              "app.kubernetes.io/instance" = name;
              "app.kubernetes.io/name" = name;
              "app.kubernetes.io/version" = "latest";
            };

            spec = {
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
                  serviceAccountName = name;
                  containers = [
                    {
                      inherit name;
                      image = cfg.image;
                      imagePullPolicy = "IfNotPresent";
                      env = [
                        {
                          name = "DOZZLE_NO_ANALYTICS";
                          value = if cfg.noAnalytics then "true" else "false";
                        }
                        {
                          name = "DOZZLE_MODE";
                          value = "k8s";
                        }
                      ]
                      ++ lib.optionals (cfg.filter != "") [
                        {
                          name = "DOZZLE_FILTER";
                          value = cfg.filter;
                        }
                      ]
                      ++ lib.optionals (cfg.level != "all") [
                        {
                          name = "DOZZLE_LEVEL";
                          value = cfg.level;
                        }
                      ];
                      ports = [
                        {
                          containerPort = cfg.service.port;
                          name = "http";
                          protocol = "TCP";
                        }
                      ];
                      volumeMounts = lib.optionals cfg.mountContainerdSocket [
                        {
                          mountPath = "/var/run/docker.sock";
                          name = "containerd-socket";
                          readOnly = true;
                        }
                      ];
                      resources = {
                        requests = {
                          memory = "64Mi";
                          cpu = "50m";
                        };
                        limits = {
                          memory = "256Mi";
                          cpu = "200m";
                        };
                      };
                    }
                  ];
                  volumes = lib.optionals cfg.mountContainerdSocket [
                    {
                      name = "containerd-socket";
                      hostPath = {
                        path = "/run/k3s/containerd/containerd.sock";
                        type = "Socket";
                      };
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
