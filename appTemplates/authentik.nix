{ lib, ... }:
with lib;
let
  postgresql-secret = "authentik-postgres-auth";
  secret-secret = "authentik-secret-key";
in
{
  name = "authentik";

  sopsSecrets = cfg: {
    ${postgresql-secret} = {
      inherit (cfg.database) password;
    };
    ${secret-secret} = {
      "authentik-secret-key" = cfg.secret-key;
    };
  };

  # https://artifacthub.io/packages/helm/goauthentik/authentik
  chart = lib.helm.downloadHelmChart {
    repo = "https://charts.goauthentik.io/";
    chart = "authentik";
    version = "2026.8.1";
    chartHash = "sha256-F4M0q7SLhMWxv3wlw03AX5GQ79pwTcNQ+2+M3xLDf3Y=";
  };

  uses-ingress = true;
  uses-database = true;

  extraOptions = {
    secret-key = mkOption {
      description = mdDoc "The secret key";
      type = types.str;
      default = "CHANGEME";
    };
  };

  defaultValues =
    cfg: with cfg; {
      authentik = {
        error_reporting.enabled = true;
        # No password here: the chart writes these values into a plain
        # Secret. It comes from global.env below instead.
        postgresql = {
          inherit (cfg.database) host name port;
          user = cfg.database.username;
        };
        secret_key = "this is a secret";
      };

      global.env = [
        {
          name = "AUTHENTIK_POSTGRESQL__PASSWORD";
          valueFrom.secretKeyRef = {
            name = postgresql-secret;
            key = "password";
          };
        }
        {
          name = "AUTHENTIK_SECRET_KEY";
          valueFrom.secretKeyRef = {
            name = secret-secret;
            key = "authentik-secret-key";
          };
        }
      ];

      # The bundled Bitnami postgres stays off; databaseTarget picks the server.
      postgresql.enabled = false;

      server.ingress = with ingress; {
        enabled = true;
        inherit ingressClassName;
        annotations = {
          "cert-manager.io/cluster-issuer" = clusterIssuer;
          "ingress.kubernetes.io/force-ssl-redirect" = "true";
          "ingress.kubernetes.io/proxy-body-size" = "0";
          "ingress.kubernetes.io/ssl-redirect" = "true";
        };
        hosts = [ domain ];
        tls = [
          {
            secretName = "authentik-tls";
            hosts = [ domain ];
          }
        ];
        https = false;
      };
    };

  extraResources = cfg: {
    middlewares.middlewares-authentik.spec.forwardAuth = {
      address = "http://authentik-server/outpost.goauthentik.io/auth/traefik";
      trustForwardHeader = true;
      authResponseHeaders = [
        "X-authentik-username"
        "X-authentik-groups"
        "X-authentik-email"
        "X-authentik-name"
        "X-authentik-uid"
        "X-authentik-jwt"
        "X-authentik-meta-jwks"
        "X-authentik-meta-outpost"
        "X-authentik-meta-provider"
        "X-authentik-meta-app"
        "X-authentik-meta-version"
      ];
    };

  };
}
