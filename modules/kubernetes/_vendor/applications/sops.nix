{ ... }:
{
  flake.nixidyApps.sops =
    {
      config,
      crdImports,
      lib,
      self,
      ...
    }:
    self.lib.mkArgoApp { inherit config lib; } rec {
      name = "sops";

      # https://artifacthub.io/packages/helm/sops-secrets-operator/sops-secrets-operator
      chart = lib.helm.downloadHelmChart {
        repo = "https://isindir.github.io/sops-secrets-operator/";
        chart = "sops-secrets-operator";
        version = "0.28.1";
        chartHash = "sha256-CQtkd43Tmu9eT5aLzueiZYBihw9nG13ewLm9dtKTk2o=";
      };

      defaultValues = cfg: {
        extraEnv = [
          {
            name = "SOPS_AGE_KEY_FILE";
            value = "/etc/sops-age-key-file/key";
          }
        ];
        secretsAsFiles = [
          {
            mountPath = "/etc/sops-age-key-file";
            name = "sops-age-key-file";
            secretName = "sops-age-key-file";
          }
        ];
      };

      extraConfig = cfg: { nixidy.applicationImports = [ (toString crdImports.sops) ]; };

      extraResources = cfg: {
        deployments = {
          "${name}-${name}-secrets-operator" = {
            spec = {
              template = {
                spec = {
                  containers = [
                    {
                      name = "${name}-secrets-operator";
                      # Less aggressive health checks for when system is under load
                      # Readiness: increased timeout from 1s to 5s, period from 10s to 20s, failures from 3 to 5
                      # Liveness: increased timeout from 1s to 10s, period from 20s to 30s, failures from 3 to 5
                      readinessProbe = lib.mkForce {
                        httpGet = {
                          path = "/readyz";
                          port = 8081;
                          scheme = "HTTP";
                        };
                        initialDelaySeconds = 10;
                        periodSeconds = 20;
                        timeoutSeconds = 5;
                        successThreshold = 1;
                        failureThreshold = 5;
                      };
                      livenessProbe = lib.mkForce {
                        httpGet = {
                          path = "/healthz";
                          port = 8081;
                          scheme = "HTTP";
                        };
                        initialDelaySeconds = 30;
                        periodSeconds = 30;
                        timeoutSeconds = 10;
                        successThreshold = 1;
                        failureThreshold = 5;
                      };
                    }
                  ];
                };
              };
            };
          };
        };
      };

      # extraResources = cfg: {
      #   sealedSecrets = {
      #     sops-age-key-file-sealed-secret = {
      #       spec = {
      #         encryptedData = {
      #           key =
      #             "AgC7egoLjUOvqAa9OdLfC0OPaqdZaGZDuv9YGvu1LrjRfBQFoZtjtRCpRu3VBwii9KzklL38iWJBTrfZW48Fb6SBtzV1dZd5dBDuAR/qA6AE8RwmYc91rrIB0Z6KJ20Qn3wwQczxiEDYmEqkV5t+nrMlsBY3Mkm458UvuG3a2uKOsjYNASp07uJjOEQB+HNEvdBP2mKC68X2tAFziMStiyUGm4D4zfVshg+JN8gjCONjW9urwdWxHDxsCR0MEEspr+szzu3Y5gLfLPCpIgegP7F7C2EIs7+pRtVEULNlwDaRho52CI4ck9gelLpKbAASYHs484D6pVxBjrlvl8EoylVgzpZjOwl2k4PyCwQJRDB5PJj2LObfTYFSs0mny7TSPETA1dF0jtuO0O2mXvIBZ8qBRm6Jabrbap62S6YwcRhjItYS6BWlUdVyXkz3aHUlNM4/QUxgJ4d6Id03VaglIoPHSAuK/tvahjnpJSGAT+FLZ+2raUAzFUgxCugPrYOhsAulyzRA7cExDRxY0QA2nn3gmTzbC0t3eL573XbKGfEYg4Dt3OwXJMaZL8ocmmUXFjIN9lry4lM1NNxS/jkGJwQcMJuG/JmKafTF+QGwypYwc0GEOMJRVIE+BUq/h/2ByEATMQMj3Tpp0udBdRK2YyYIlwu7+riqL/Y9vC19gOVeIMSer1XzBcQb6S9bN7kOh/Odv7JA0QLfyRgvYJlS3zzQ4x/oKJlirtPthcUjt9Ghb3RbHfwTysjcR1cYX423YHnDaDqz2IQ0dmiRUxFZnUZ7dIfai04d+JNVgQ==";
      #         };

      #         template.metadata = {
      #           name = "sops-age-key-file";
      #           namespace = "sops";
      #         };
      #       };
      #     };
      #   };
      # };
    };
}
