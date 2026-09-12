{ ... }:
{
  flake.nixidyApps.memos =
    {
      config,
      lib,
      pkgs,
      self,
      ...
    }:
    with lib;
    let
      db-secret = "memos-database";
      # Memos parses MEMOS_DSN with net/url; userinfo must be percent-encoded (passwords with : ) @ ; etc.).
      enc = pkgs.lib.escapeURL;
      postgresDsn =
        cfg:
        "postgresql://${enc cfg.database.username}:${enc cfg.database.password}@${cfg.database.host}:${toString cfg.database.port}/${cfg.database.name}?sslmode=disable";
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
      rec {
        name = "memos";

        # https://artifacthub.io/packages/helm/gabe565/memos
        chart = lib.helm.downloadHelmChart {
          repo = "https://charts.gabe565.com";
          chart = "memos";
          version = "0.17.0";
          chartHash = "sha256-n71ue+gWD+gDb/nRep1//k2OgIE0bdguv8ze/8qm4tI=";
        };

        uses-ingress = true;
        uses-database = true;

        sopsSecrets =
          cfg:
          optionalAttrs (cfg.database.password != "") {
            ${db-secret}.memos-dsn = postgresDsn cfg;
          };

        defaultValues =
          cfg:
          {
            ingress.main = with cfg.ingress; {
              enabled = false;
              hosts = [
                {
                  host = domain;
                  paths = [ { path = "/"; } ];
                }
              ];
              tls = [
                {
                  secretName = "memo-tls";
                  hosts = [ domain ];
                }
              ];
            };
            persistence.data.enabled = false;
            postgresql.enabled = false;
          }
          // optionalAttrs (cfg.database.password != "") {
            env = {
              MEMOS_DRIVER = "postgres";
              MEMOS_DSN = {
                valueFrom.secretKeyRef = {
                  name = db-secret;
                  key = "memos-dsn";
                };
              };
            };
          };

        extraResources =
          cfg: with cfg; {
            ingresses = with cfg.ingress; {
              memos = {
                metadata.annotations = optionalAttrs (clusterIssuer != "") {
                  "cert-manager.io/cluster-issuer" = clusterIssuer;
                };

                spec = {
                  inherit (cfg.ingress) ingressClassName;
                  rules = [
                    {
                      host = domain;
                      http = {
                        paths = [
                          {
                            path = "/";
                            pathType = "ImplementationSpecific";
                            backend.service = {
                              inherit name;
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
                      secretName = tls.secretName;
                    }
                  ];
                };
              };
            };
          };
      };
}
