{ ... }:
{
  flake.nixidyApps.openldap =
    { config, lib, ... }:
    let
      cfg = config.services.openldap;

      chart = lib.helm.downloadHelmChart {
        repo = "https://charts.rock8s.com";
        chart = "openldap";
        version = "4.1.1";
        chartHash = "sha256-KXaKUUkqmg66urgTybvSNH67FjrJEt68sRWP2gFSM98=";
      };

      defaultNamespace = "openldap";
      domain = "ldap.dev.kronkltd.net";

      clusterIssuer = "letsencrypt-prod";

      defaultValues = {
        replicaCount = 1;
        openldap.hostname = domain;
        tls.secret = "openldap-tls";
        env = {
          LDAP_ORGANISATION = "KRONK Ltd.";
          LDAP_DOMAIN = domain;
        };
        ingress.phphldapadmin = {
          certificate = "phpldapadmin-tls";
          enabled = true;
          hostname = "phpldapadmin.dev.kronkltd.net";
        };
        phpldapadmin.ingress = {
          annotations = {
            "cert-manager.io/cluster-issuer" = clusterIssuer;
            "ingress.kubernetes.io/force-ssl-redirect" = "true";
            "ingress.kubernetes.io/proxy-body-size" = "0";
            "ingress.kubernetes.io/ssl-redirect" = "true";
          };
          hosts = "phpldapadmin.dev.kronkltd.net";
        };

        ltb-passwd.ingress = {
          annotations = {
            "cert-manager.io/cluster-issuer" = clusterIssuer;
            "ingress.kubernetes.io/force-ssl-redirect" = "true";
            "ingress.kubernetes.io/proxy-body-size" = "0";
            "ingress.kubernetes.io/ssl-redirect" = "true";
          };
          hosts = [ "ltb.dev.kronkltd.net" ];
        };
      };

      values = lib.attrsets.recursiveUpdate defaultValues cfg.values;
      namespace = cfg.namespace;
    in
    with lib;
    {
      options.services.openldap = {
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
        applications.openldap = {
          inherit namespace;
          createNamespace = true;
          finalizer = "foreground";
          helm.releases.openldap = { inherit chart values; };

          resources = {
            sealedSecrets = {
              openldap-passwords = {
                spec = {
                  encryptedData = {
                    LDAP_ADMIN_PASSWORD = "AgBkRKENFTTbii/ArxXY55jXeeH4fXlrgMB5CDUKB5zUE5wuSkYLktT3uRc7ki+a4/nrISgpcSvl0PAbXm6nQqdi/IgD3Sqz7MOSfzpp6oLkzqYy1iCGwiqufglPy1LeH8M3BfW0spRi/KzslespiwD/T6WBCwuQBcAzIoOzYOoePu9ixDPCwus1zp0xKX+tfxVIJVbOcIToOU2lIoSdAY79xKj9MuM/jo7OHDryzaHLR40aoDtKB/0OB47Z36WUuqLudSN6FpZFZ3Ew8DFqohXUDSA0FuNjAUpP7Ku2IzikIg2E6hq6+d3sdesQ/Sjrw8duGVSoVqiHqj0s0KUwt9N4lWXBdKv41LAB3fb76dCtPH0c4maMiO7HC9QOGl0LIzGe+ek1Y4u7Q3Pixz/lZwSAs+GpxDYQmpSrjkxa39su8LoWDr+DeMl5Qfls541gKjZ1enHzEKbf/10W2Id6mtuzK5BeRMML5WN9uatyhz2FPxMsH/Nr2Xjn2fcjGT4c/qyHb2XpuqaiW/icvKBnA/jfNACuQoZd5PE6jSGXH16uKCGjL/aW0F0EXbUyTz+VDmZx36gGGaUozicA3TJtY8Hrku1W1OyeVDgh6h2yUjOU/MASJV5hgttqx+SrcxPh1fU33ldzl+s42F4nuf7PDoFKQMKEDGrVn9x/endwXplg3+DQZoSrUY6xKr/JST7+sYeaMGqIq+3T97Ouib9iFkzCVh2nxw==";
                    LDAP_CONFIG_PASSWORD = "AgByfIz4T6D4tx4LgW3Z0hZh1t1U++Ll4FyDI/qsftZLgBZ7C7RX2cof8lDIAjAgiYpxwOPHRbNkjzQ3r2dsEF6nsM88YVdVxqK3UGqDYiDM6AqgjIQISyOXNnADCT1yuj1ORxTxT5Os9WjjlqrP2qqHkKhhMZMcjJBoK//CtKKt24rXSU1djbOMFSo7vYWpm/WTx+sMLgYKSP0Qq5XlmgDJ33K9Mb0aU2lr+UYSYbkCySrmq/6LEWjWa1xFBxtNHtpN5VN8y+NHDNCHI404zhjNeo+BwJvHJ85Ck5Nv4822MdKC1cjLVt3fnjQeMVLF3kpye91ByZMQcFQ/K25y7fZhU8WyH3JN6IadqrN9ZJzUFd3nff0Z/nG9FKkPlxigo136Lw5MF2cuxSWQYCEmOj24clriStJjfEWozWmEGqnWRyVrvNCV72RBpqi2Ympe3UfYZkXjTTZdlLxYbEYWQuosRE0jdVYfDHLkE4aDnUz8xyJvrh4xKJdP9jJ4fzEGr4MGJqNgoqshyWi/TEha4G0kKFSKsaQMPw/MmVsMqVUiYdzBvafA7Jrr5x2UQHAxHv8Nyjj1zDUU1Vxi6XqgM36Rsg7CZjPezAIRHTICZHnpaoAy1bwDk+gAlQlWqDRCG4i5YZINyvtXrKCCki3RQ0/fGVARDXZENidew2K2v9ZqQVY1V+xvTGa7SVMCr78f8pK8YEA3Jor2Xr5fW2vvOJJSPLv1WQ==";
                  };
                  template.metadata = {
                    inherit namespace;
                    name = "openldap-passwords";
                  };
                };
              };
            };
          };

          syncPolicy.finalSyncOpts = [ "CreateNamespace=true" ];
        };
      };
    };
}
