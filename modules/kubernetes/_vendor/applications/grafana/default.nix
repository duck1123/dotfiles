{ ... }:
{
  flake.nixidyApps.grafana =
    {
      charts,
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      grafana-secret = "grafana-admin";
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
        name = "grafana";

        sopsSecrets = cfg: {
          ${grafana-secret} = {
            "admin-user" = cfg.adminUser or "admin";
            "admin-password" = cfg.adminPassword or "";
          };
        };

        # https://artifacthub.io/packages/helm/grafana/grafana
        chart = charts.grafana.grafana;

        uses-ingress = true;

        defaultValues = cfg: {
          admin = {
            existingSecret = grafana-secret;
            userKey = "admin-user";
            passwordKey = "admin-password";
          };

          dashboardProviders."dashboardproviders.yaml" = {
            apiVersion = 1;
            providers = cfg.additionalDashboardProviders;
          };

          dashboards =
            lib.recursiveUpdate
              {
                default.system-performance-nfs.json = builtins.readFile ./dashboards/system-performance.json;
                default.node-overview.json = builtins.readFile ./dashboards/node-overview.json;
              }
              (
                lib.optionalAttrs cfg.enableLogging {
                  default.pod-logs.json = builtins.readFile ./dashboards/pod-logs.json;
                }
                // (cfg.additionalDashboards or { })
              );

          datasources."datasources.yaml" = {
            apiVersion = 1;
            datasources =
              if cfg.enableLogging then
                cfg.additionalDatasources
              else
                builtins.filter (ds: ds.type != "loki") cfg.additionalDatasources;
          };

          ingress = with cfg.ingress; {
            inherit ingressClassName;

            annotations = lib.optionalAttrs (clusterIssuer != "") {
              "cert-manager.io/cluster-issuer" = clusterIssuer;
            };

            enabled = true;
            hosts = [ domain ];

            tls = [
              {
                secretName = "grafana-tls";
                hosts = [ domain ];
              }
            ];
          };

          nodeSelector."kubernetes.io/hostname" = cfg.hostAffinity;

          persistence = {
            enabled = true;
            storageClassName = cfg.storageClassName;
          };

          resources = {
            requests = {
              cpu = "100m";
              memory = "128Mi";
            };
            limits = {
              cpu = "500m";
              memory = "512Mi";
            };
          };

          grafana.ini =
            lib.recursiveUpdate
              {
                server = {
                  domain = cfg.ingress.domain;
                  root_url = "https://${cfg.ingress.domain}";
                };
              }
              (
                lib.optionalAttrs (cfg.enableProxyAuth or false) {
                  "auth.proxy" = {
                    enabled = "true";
                    header_name = "X-WebAuth-User";
                    header_property = "username";
                    auto_sign_up = "true";
                    enable_login_token = "true";
                  };
                }
              );
        };

        extraOptions = {
          adminUser = mkOption {
            description = mdDoc "Grafana admin username";
            type = types.str;
            default = "admin";
          };

          adminPassword = mkOption {
            description = mdDoc "Grafana admin password";
            type = types.str;
            default = "";
          };

          enableProxyAuth = mkOption {
            description = mdDoc "Enable proxy authentication for Tailscale proxy-to-grafana. If enabled, authentication is handled via Tailscale identity headers. If disabled, use normal username/password login.";
            type = types.bool;
            default = false;
          };

          additionalDatasources = mkOption {
            description = mdDoc "Datasources to provision. List of datasource configuration objects.";
            type = types.listOf types.attrs;
            default = [ ];
            example = [
              {
                name = "Prometheus";
                type = "prometheus";
                access = "proxy";
                url = "http://prometheus:9090";
                isDefault = true;
                editable = true;
                jsonData.httpMethod = "POST";
              }
            ];
          };

          additionalDashboardProviders = mkOption {
            description = mdDoc "Dashboard providers to provision. List of dashboard provider configuration objects.";
            type = types.listOf types.attrs;
            default = [ ];
            example = [
              {
                name = "default";
                orgId = 1;
                folder = "";
                type = "file";
                disableDeletion = false;
                editable = true;
                options.path = "/var/lib/grafana/dashboards/default";
              }
            ];
          };

          additionalDashboards = mkOption {
            description = mdDoc "Dashboards to provision. Attribute set mapping folder names to dashboards (folder name -> dashboard file name -> dashboard JSON content)";
            example.default."my-dashboard.json" = builtins.readFile ./path/to/dashboard.json;
            type = types.attrsOf (types.attrsOf types.str);
            default = { };
          };

          enableLogging = mkOption {
            description = mdDoc "Enable logging features (Loki datasource and log-related dashboards)";
            type = types.bool;
            default = true;
          };
        };
      };
}
