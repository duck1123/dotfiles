{ ... }:
{
  flake.nixidyApps.opensearch-dashboards =
    {
      config,
      lib,
      self,
      ...
    }:
    with lib;
    self.lib.mkArgoApp { inherit config lib self; } (
      let
        name = "opensearch-dashboards";
        labels."app.kubernetes.io/name" = name;
        port = 5601;
      in
      {
        inherit name;
        uses-ingress = true;

        extraOptions = {
          image = mkOption {
            description = mdDoc "The opensearch-dashboards docker image";
            type = types.str;
            # Kept in step with the OpenSearch server version it points at
            # (currently ditto-relay's bundled instance -- see
            # applications/ditto-relay.nix) since Dashboards doesn't support
            # talking to a server on a different major version.
            default = "opensearchproject/opensearch-dashboards:2.19.6";
          };

          opensearchHosts = mkOption {
            description = mdDoc "OPENSEARCH_HOSTS -- URLs of the OpenSearch cluster(s) to query, e.g. [ \"http://ditto-relay-opensearch.ditto-relay:9200\" ].";
            type = types.listOf types.str;
            default = [ ];
          };
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
                    env = [
                      {
                        name = "OPENSEARCH_HOSTS";
                        value = builtins.toJSON cfg.opensearchHosts;
                      }
                      {
                        # The backends this points at run with their own
                        # security plugin disabled (see
                        # applications/ditto-relay.nix's DISABLE_SECURITY_PLUGIN) --
                        # Dashboards needs the matching flag or it fails to connect.
                        name = "DISABLE_SECURITY_DASHBOARDS_PLUGIN";
                        value = "true";
                      }
                    ];
                    ports = [
                      {
                        containerPort = port;
                        name = "http";
                        protocol = "TCP";
                      }
                    ];
                    readinessProbe = {
                      httpGet = {
                        path = "/api/status";
                        port = port;
                      };
                      initialDelaySeconds = 20;
                      periodSeconds = 10;
                      timeoutSeconds = 5;
                      failureThreshold = 6;
                    };
                    livenessProbe = {
                      httpGet = {
                        path = "/api/status";
                        port = port;
                      };
                      initialDelaySeconds = 30;
                      periodSeconds = 30;
                      timeoutSeconds = 5;
                      failureThreshold = 3;
                    };
                    resources = {
                      requests = {
                        memory = "512Mi";
                        cpu = "100m";
                      };
                      limits = {
                        memory = "1Gi";
                        cpu = "1000m";
                      };
                    };
                  }
                ];
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
            selector = labels;
            ports = [
              {
                name = "http";
                port = port;
                targetPort = port;
                protocol = "TCP";
              }
            ];
          };
        };
      }
    );
}
