{ ... }:
{
  flake.nixidyApps.elasticvue =
    {
      config,
      lib,
      self,
      ...
    }:
    with lib;
    self.lib.mkArgoApp { inherit config lib self; } (
      let
        name = "elasticvue";
        labels."app.kubernetes.io/name" = name;
        clustersSecret = "elasticvue-clusters";
      in
      {
        inherit name;
        uses-ingress = true;

        extraOptions = {
          image = mkOption {
            description = mdDoc "The elasticvue docker image";
            type = types.str;
            default = "cars10/elasticvue:1.15.0";
          };

          service.port = mkOption {
            description = mdDoc "The service port";
            type = types.int;
            default = 8080;
          };

          # Pre-seeds the "add cluster" list shown to every browser that opens
          # this instance -- elasticvue itself only ever stores connections in
          # the *browser's* local storage (see ELASTICVUE_CLUSTERS in the
          # cars10/elasticvue README), so without this each device would need
          # the same clusters re-entered by hand.
          clusters = mkOption {
            description = mdDoc "Elasticsearch/OpenSearch backends to pre-populate via ELASTICVUE_CLUSTERS.";
            type = types.listOf (
              types.submodule {
                options = {
                  name = mkOption {
                    type = types.str;
                  };
                  uri = mkOption {
                    type = types.str;
                  };
                  username = mkOption {
                    type = types.str;
                    default = "";
                  };
                  password = mkOption {
                    type = types.str;
                    default = "";
                  };
                };
              }
            );
            default = [ ];
          };
        };

        sopsSecrets =
          cfg:
          optionalAttrs (cfg.clusters != [ ]) {
            ${clustersSecret}.ELASTICVUE_CLUSTERS = builtins.toJSON (
              map (
                c:
                { inherit (c) name uri; }
                // optionalAttrs (c.username != "") { inherit (c) username; }
                // optionalAttrs (c.password != "") { inherit (c) password; }
              ) cfg.clusters
            );
          };

        extraResources = cfg: {
          deployments.${name}.spec = {
            selector.matchLabels = labels;
            template = {
              metadata.labels = labels;
              spec = {
                containers = [
                  {
                    inherit name;
                    image = cfg.image;
                    imagePullPolicy = "IfNotPresent";
                    env = lib.optionals (cfg.clusters != [ ]) [
                      {
                        name = "ELASTICVUE_CLUSTERS";
                        valueFrom.secretKeyRef = {
                          name = clustersSecret;
                          key = "ELASTICVUE_CLUSTERS";
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
                      initialDelaySeconds = 5;
                      periodSeconds = 10;
                      timeoutSeconds = 5;
                      failureThreshold = 3;
                    };
                    resources = {
                      requests = {
                        memory = "32Mi";
                        cpu = "25m";
                      };
                      limits = {
                        memory = "128Mi";
                        cpu = "200m";
                      };
                    };
                  }
                ];
              };
            };
          };

          ingresses.${name} = {
            metadata.annotations = optionalAttrs (cfg.ingress.clusterIssuer != "") {
              "cert-manager.io/cluster-issuer" = cfg.ingress.clusterIssuer;
            };

            spec = with cfg.ingress; {
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
                  secretName = "${domain}-tls";
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
                targetPort = cfg.service.port;
                protocol = "TCP";
              }
            ];
          };
        };
      }
    );
}
