{ ... }:
{
  # https://www.keycloak.org/
  flake.nixidyApps.keycloak =
    {
      config,
      lib,
      self,
      ...
    }:
    with lib;
    self.lib.mkArgoApp { inherit config lib; } {
      name = "keycloak";

      # https://github.com/codecentric/helm-charts/tree/master/charts/keycloakx
      # (bitnami/keycloak was frozen behind Bitnami's Aug 2025 paid-tier
      # restructuring; codecentric is the actively-maintained community chart)
      chart = lib.helm.downloadHelmChart {
        repo = "https://codecentric.github.io/helm-charts";
        chart = "keycloakx";
        version = "7.3.1";
        chartHash = "sha256-JyGehtXvxeMebHhFi2rx2TkTToT/Id8uICBwNfJkvGI=";
      };

      uses-ingress = true;

      extraOptions = {
        ingress = {
          adminDomain = mkOption {
            description = mdDoc "The ingress domain for the admin console";
            type = types.str;
            default = "keycloak-admin.local";
          };
        };

        auth = {
          adminPassword = mkOption {
            description = mdDoc "The admin console password";
            type = types.str;
            default = "CHANGEME";
          };
        };

        # External database Keycloak stores its realm/user data in. Fill
        # these in via env/dev/keycloak.nix (following the pattern in
        # env/dev/postgresql.nix / env/dev/mariadb.nix) before enabling.
        database = {
          vendor = mkOption {
            description = mdDoc "Database vendor: dev-file, dev-mem, mariadb, mssql, mysql, oracle or postgres";
            type = types.str;
            default = "postgres";
          };

          host = mkOption {
            description = mdDoc "Database host";
            type = types.str;
            default = "";
          };

          port = mkOption {
            description = mdDoc "Database port";
            type = types.port;
            default = 5432;
          };

          name = mkOption {
            description = mdDoc "Database name";
            type = types.str;
            default = "keycloak";
          };

          username = mkOption {
            description = mdDoc "Database username";
            type = types.str;
            default = "keycloak";
          };

          password = mkOption {
            description = mdDoc "Database password";
            type = types.str;
            default = "CHANGEME";
          };
        };
      };

      sopsSecrets = cfg: {
        keycloak-admin-password = {
          password = cfg.auth.adminPassword;
        };
        keycloak-db-password = with cfg.database; {
          inherit password;
        };
      };

      defaultValues = cfg: {
        database = with cfg.database; {
          inherit vendor;
          hostname = host;
          inherit port;
          database = name;
          inherit username;
          existingSecret = "keycloak-db-password";
          existingSecretKey = "password";
        };

        extraEnv = ''
          - name: KEYCLOAK_ADMIN
            value: admin
          - name: KEYCLOAK_ADMIN_PASSWORD
            valueFrom:
              secretKeyRef:
                name: keycloak-admin-password
                key: password
        '';

        ingress = with cfg.ingress; {
          enabled = true;
          ingressClassName = "traefik";
          annotations = {
            "cert-manager.io/cluster-issuer" = clusterIssuer;
            "ingress.kubernetes.io/force-ssl-redirect" = "true";
          };
          tls = [
            {
              hosts = [ domain ];
              secretName = "keycloak-tls";
            }
          ];
          rules = [
            {
              host = domain;
              paths = [
                {
                  path = "/";
                  pathType = "Prefix";
                }
              ];
            }
          ];

          console = {
            enabled = true;
            ingressClassName = "traefik";
            annotations = {
              "cert-manager.io/cluster-issuer" = "letsencrypt-prod";
              "ingress.kubernetes.io/force-ssl-redirect" = "true";
            };
            rules = [
              {
                host = adminDomain;
                paths = [
                  {
                    path = "/admin";
                    pathType = "Prefix";
                  }
                ];
              }
            ];
          };
        };
      };
    };
}
