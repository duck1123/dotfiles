{ ... }:
{
  flake.nixidyApps.marquez =
    {
      config,
      lib,
      ...
    }:
    let
      app-name = "marquez";
      cfg = config.services.${app-name};

      # https://artifacthub.io/packages/helm/ilum/ilum-marquez
      chart = lib.helm.downloadHelmChart {
        repo = "https://charts.ilum.cloud";
        chart = "ilum-marquez";
        version = "0.42.0";
        chartHash = "sha256-9Z3f8rvSkisrY9b466MiQYgqeHCY5AvjKq7Q8aJ3OKg=";
      };

      values = lib.attrsets.recursiveUpdate {
        ingress = {
          enabled = false;
          hosts = [ cfg.domain ];
        };

        marquez = {
          hostname = cfg.domain;
        };

        web = {
          enabled = true;
        };
      } cfg.values;
    in
    with lib;
    {
      options.services.${app-name} = {
        domain = mkOption {
          description = mdDoc "The domain";
          type = types.str;
          default = "${app-name}.localhost";
        };

        enable = mkEnableOption "Enable application";

        namespace = mkOption {
          description = mdDoc "The namespace to install into";
          type = types.str;
          default = app-name;
        };

        values = mkOption {
          description = "All the values";
          type = types.attrsOf types.anything;
          default = { };
        };
      };

      config = mkIf cfg.enable {
        applications.${app-name} = {
          inherit (cfg) namespace;
          createNamespace = true;
          finalizer = "foreground";
          helm.releases.${app-name} = { inherit chart values; };

          resources = {
            ingresses.imum-marquez-ingress.spec = {
              ingressClassName = "traefik";
              rules = [
                {
                  host = cfg.domain;
                  http = {
                    paths = [
                      {
                        path = "/api/";
                        pathType = "Prefix";
                        backend.service = {
                          name = "imum-marquez-web";
                          port.name = "http";
                        };
                      }
                    ];
                  };
                }
              ];

              tls = [
                {
                  hosts = [ cfg.domain ];
                  secretName = "marquez-tls";
                }
              ];
            };
          };

          syncPolicy.finalSyncOpts = [ "CreateNamespace=true" ];
        };
      };
    };
}
