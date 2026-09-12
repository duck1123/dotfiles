{ ... }:
{
  flake.nixidyApps.homer =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    self.lib.mkArgoApp { inherit config lib; } {
      name = "homer";

      # https://artifacthub.io/packages/helm/gabe565/homer
      chart = helm.downloadHelmChart {
        repo = "https://charts.gabe565.com";
        chart = "homer";
        version = "0.13.0";
        chartHash = "sha256-z6o5LHUYqm7Jd5gsIs+J3Z48Frbj8F1ZnEZw4mHIeQA=";
      };

      uses-ingress = true;

      extraOptions = {
        codeserver.ingress = {
          clusterIssuer = mkOption {
            description = mdDoc "The cookie secret";
            type = types.str;
            default = "CHANGEME";
          };

          domain = mkOption {
            description = mdDoc "The cookie secret";
            type = types.str;
            default = "CHANGEME";
          };

          enable = mkEnableOption "Enable codeserver addon";

          ingressClassName = mkOption {
            description = mdDoc "The cookie secret";
            type = types.str;
            default = "CHANGEME";
          };
        };
      };

      defaultValues = cfg: {
        configMaps = {
          config = {
            enable = true;

            data = {
              "config.yml" = self.lib.toYAML {
                inherit pkgs;
                value = {
                  title = "App Dashboard";
                  columns = 4;

                  defaults = {
                    colorTheme = "dark";
                  };

                  links = [
                    {
                      name = "Nostrudel";
                      icon = "fab fa-github";
                      url = "https://nostrudel.ninja/";
                      target = "_blank";
                    }
                  ];
                };
              };
            };
          };
        };

        ingress = with cfg.ingress; {
          main = {
            enabled = false;
            hosts = [
              {
                host = domain;
                paths = [ { path = "/"; } ];
              }
            ];
            tls = [
              {
                secretName = tls.secretName;
                hosts = [ domain ];
              }
            ];
          };

          addons.codeserver = with cfg.codeserver.ingress; {
            enabled = enable;

            ingress = {
              enabled = false;

              hosts = [
                {
                  host = domain;
                  paths = [ { path = "/"; } ];
                }
              ];
              tls = [
                {
                  secretName = "codeserver-tls";
                  hosts = [ domain ];
                }
              ];
            };
          };

        };

        persistence = {
          config = {
            enabled = true;
            storageClass = cfg.storageClassName;
          };
        };
      };

      extraResources = cfg: {
        ingresses = with cfg.ingress; {
          homer.spec = {
            inherit ingressClassName;
            rules = [
              {
                host = domain;
                http = {
                  paths = [
                    {
                      path = "/";
                      pathType = "ImplementationSpecific";
                      backend.service = {
                        name = "homer";
                        port.name = "http";
                      };
                    }
                  ];
                };
              }
            ];
            tls = [
              {
                hosts = [ domain ];
                secretName = "homer-tls";
              }
            ];
          };
        };
      };
    };
}
