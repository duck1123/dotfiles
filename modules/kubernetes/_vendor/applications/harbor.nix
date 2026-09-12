{ ... }:
{
  flake.nixidyApps.harbor =
    {
      config,
      lib,
      ...
    }:
    let
      app-name = "harbor";
      cfg = config.services.${app-name};
      defaultNamespace = "harbor";
      registry-domain = "registry.dev.kronkltd.net";
    in
    with lib;
    {
      options.services.${app-name} = {
        clusterIssuer = mkOption {
          description = mdDoc "The issuer";
          type = types.str;
          default = "letsencrypt-prod";
        };

        domain = mkOption {
          description = mdDoc "The domain";
          type = types.str;
          default = "harbor.localhost";
        };

        enable = mkEnableOption "Enable application";

        namespace = mkOption {
          description = mdDoc "The namespace to install into";
          type = types.str;
          default = defaultNamespace;
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
          # helm.releases.${app-name} = { inherit chart values; };

          resources = {
            ingresses.harbor-registry-direct = {
              metadata = {
                annotations = {
                  "cert-manager.io/cluster-issuer" = "letsencrypt-prod";
                  "traefik.ingress.kubernetes.io/router.entrypoints" = "websecure";
                  "traefik.ingress.kubernetes.io/router.middlewares" =
                    "harbor-harbor-allow-large-upload@kubernetescrd";
                };
              };

              spec = {
                tls = [
                  {
                    hosts = [ registry-domain ];
                    secretName = "harbor-registry-tls";
                  }
                ];
                rules = [
                  {
                    host = registry-domain;
                    http.paths = [
                      {
                        path = "/";
                        pathType = "ImplementationSpecific";
                        backend.service = {
                          name = "harbor-core";
                          port.number = 80;
                        };
                      }
                    ];
                  }
                ];
              };
            };

            middlewares.allow-large-upload.spec.buffering = {
              maxRequestBodyBytes = 10737418240;
              maxResponseBodyBytes = 0;
              memRequestBodyBytes = 10485760;
              memResponseBodyBytes = 10485760;
            };
          };

          syncPolicy.finalSyncOpts = [ "CreateNamespace=true" ];
        };
      };
    };
}
