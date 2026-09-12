{ ... }:
{
  flake.nixidyApps.forgejo =
    {
      charts,
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
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
        name = "forgejo";

        sopsSecrets = cfg: {
          forgejo-admin-password = { inherit (cfg.admin) password username; };
          postgresql-password = with cfg.postgresql; {
            inherit
              adminPassword
              adminUsername
              replicationPassword
              userPassword
              ;
          };
        };

        # `pvcName = "gitea-shared-storage"` to match the chart's own default
        # claimName (persistence.create = false below, so nothing else creates
        # this PVC). Shape only -- no volumeHandle here, that's
        # environment-specific (see env/dev/forgejo.nix and docs/pinned-volumes.md).
        volumes = cfg: {
          data = {
            pvcName = "gitea-shared-storage";
            size = "10Gi";
          };
        };

        # https://artifacthub.io/packages/helm/forgejo-helm/forgejo
        chart = charts.forgejo-helm.forgejo;

        uses-ingress = true;

        extraOptions = {
          admin = {
            password = mkOption {
              description = mdDoc "The admin password";
              type = types.str;
              default = "CHANGEME";
            };

            username = mkOption {
              description = mdDoc "The admin username";
              type = types.str;
              default = "admin";
            };
          };

          postgresql = {
            adminPassword = mkOption {
              description = mdDoc "The admin password";
              type = types.str;
              default = "CHANGEME";
            };

            adminUsername = mkOption {
              description = mdDoc "The admin username";
              type = types.str;
              default = "postgres";
            };

            replicationPassword = mkOption {
              description = mdDoc "The replication password";
              type = types.str;
              default = "CHANGEME";
            };

            userPassword = mkOption {
              description = mdDoc "The user password";
              type = types.str;
              default = "CHANGEME";
            };
          };
        };

        defaultValues = cfg: {
          gitea = {
            additionalConfigFromEnvs = [
              {
                name = "FORGEJO__DATABASE__PASSWD";
                valueFrom.secretKeyRef = {
                  key = "adminPassword";
                  name = "postgresql-password";
                };
              }
            ];

            admin.existingSecret = "forgejo-admin-password";

            config.database = {
              DB_TYPE = "postgres";
              HOST = "postgresql.postgresql:5432";
              USER = "postgres";
              NAME = "gitea";
              SCHEMA = "public";
            };

            metrics.enabled = true;
          };

          ingress = with cfg.ingress; {
            annotations = {
              "cert-manager.io/cluster-issuer" = clusterIssuer;
              "ingress.kubernetes.io/force-ssl-redirect" = "true";
            };
            className = ingressClassName;
            enabled = true;
            hosts = [
              {
                host = domain;
                paths = [
                  {
                    path = "/";
                    pathType = "ImplementationSpecific";
                  }
                ];
              }
            ];
            tls = [
              {
                hosts = [ domain ];
                secretName = "forgejo-tls";
              }
            ];
          };

          persistence = {
            storageClass = cfg.storageClassName;
            # Pinned via the `volumes` parameter above instead of letting the
            # chart create the PVC -- see docs/pinned-volumes.md. claimName
            # stays at its default ("gitea-shared-storage") to match.
            create = false;
          };
          postgresql.enabled = false;
          postgresql-ha.enabled = false;
          redis.enabled = false;
          redis-cluster.enabled = false;
        };
      };
}
