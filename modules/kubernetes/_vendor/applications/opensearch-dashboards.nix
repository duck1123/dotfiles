{ ... }:
{
  flake.nixidyApps.opensearch-dashboards =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      name = "opensearch-dashboards";
      labels."app.kubernetes.io/name" = name;
      port = 5601;

      # nix-csi runtime for the register-index-patterns job: the dashboards
      # image has neither curl nor jq, and there's no Dockerfile of ours to
      # add them to -- same pattern as applications/metabase.nix.
      toolsExpr = ''
        let
          pkgs = import (builtins.fetchTree {
            type = "github";
            owner = "nixos";
            repo = "nixpkgs";
            ref = "nixos-unstable";
          }) {};
        in
        pkgs.symlinkJoin {
          name = "opensearch-dashboards-register-index-patterns-tools";
          paths = [ pkgs.bash pkgs.curl pkgs.jq pkgs.coreutils ];
        }
      '';

      registerIndexPatternsScript = ''
        set -euo pipefail

        base_url="http://${name}.${name}"

        echo "Waiting for OpenSearch Dashboards to become healthy..."
        until curl -sf "$base_url/api/status" >/dev/null; do
          sleep 3
        done

        echo "$INDEX_PATTERNS_JSON" | jq -c '.[]' | while read -r ip; do
          id=$(echo "$ip" | jq -r '.id')
          attrs=$(echo "$ip" | jq -c '.attributes')

          echo "Registering index pattern \"$(echo "$ip" | jq -r '.attributes.title')\" (id=$id)"
          curl -sf -X POST "$base_url/api/saved_objects/index-pattern/$id?overwrite=true" \
            -H "osd-xsrf: true" -H "Content-Type: application/json" \
            -d "$(jq -n --argjson attrs "$attrs" '{attributes:$attrs}')" >/dev/null
        done
      '';
    in
    self.lib.mkArgoApp
      {
        inherit
          config
          lib
          pkgs
          self
          ;
      }
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

          indexPatterns = mkOption {
            description = mdDoc ''
              OpenSearch index patterns to register as Dashboards "Index
              Pattern" saved objects on startup -- Dashboards has no
              env-var/config-file mechanism for these, so without this a
              backend with data in it (e.g. ditto-relay's `nostr-events`
              index) shows nothing until someone creates the pattern by hand
              under Stack Management -> Index Patterns. Registered
              declaratively by the
              opensearch-dashboards-register-index-patterns job, which calls
              the saved objects REST API (POST
              /api/saved_objects/index-pattern/<id>?overwrite=true).
            '';
            type = types.listOf (
              types.submodule {
                options = {
                  title = mkOption {
                    type = types.str;
                    description = mdDoc "Index pattern title, e.g. \"nostr-events*\".";
                  };
                  timeFieldName = mkOption {
                    type = types.nullOr types.str;
                    default = null;
                    description = mdDoc "Time field for this pattern, if any (e.g. \"created_at\"). Leave null for patterns with no time field.";
                  };
                };
              }
            );
            default = [ ];
          };
        };

        extraResources =
          cfg:
          {
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
          }
          // optionalAttrs (cfg.indexPatterns != [ ]) {
            jobs."${name}-register-index-patterns" = {
              metadata.annotations = {
                "argocd.argoproj.io/hook" = "Sync";
                "argocd.argoproj.io/hook-delete-policy" = "BeforeHookCreation,HookSucceeded";
                # Runs after the Deployment above (wave "0", implicit) is created --
                # the job itself polls /api/status so it tolerates the pod not being
                # Ready yet, it just needs the Service to exist to resolve.
                "argocd.argoproj.io/sync-wave" = "1";
              };
              spec = {
                backoffLimit = 3;
                template.spec = {
                  restartPolicy = "OnFailure";
                  containers = [
                    {
                      name = "register-index-patterns";
                      image = "ghcr.io/lillecarl/nix-csi/scratch:1.0.1";
                      command = [
                        "bash"
                        "-c"
                        registerIndexPatternsScript
                      ];
                      env = [
                        {
                          name = "INDEX_PATTERNS_JSON";
                          value = builtins.toJSON (
                            map (p: {
                              # Stable id derived from the title so re-runs overwrite
                              # the same saved object instead of piling up duplicates.
                              id = builtins.replaceStrings [ "*" ] [ "" ] p.title;
                              attributes = {
                                title = p.title;
                              }
                              // optionalAttrs (p.timeFieldName != null) {
                                timeFieldName = p.timeFieldName;
                              };
                            }) cfg.indexPatterns
                          );
                        }
                      ];
                      volumeMounts = [
                        {
                          name = "nix";
                          mountPath = "/nix";
                          subPath = "nix";
                        }
                      ];
                    }
                  ];
                  volumes = [
                    {
                      name = "nix";
                      csi = {
                        driver = "nix.csi.store";
                        volumeAttributes.nixExpr = toolsExpr;
                      };
                    }
                  ];
                };
              };
            };
          };
      };
}
