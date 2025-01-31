{
  description = "kubernetes config";

  inputs.kubenix.url = "github:hall/kubenix";

  outputs = { kubenix, self, ... }@inputs:
    let
      domain = "argocd.dev.kronkltd.net";
      system = "x86_64-linux";
      bucket-name = "argo-bucket";
      endpoint = "minio.minio:9000";
      secret-name = "minio-password";
      access-secret-key = "user";
      secret-secret-key = "password";
    in rec {
      packages.${system}.default = (kubenix.evalModules.${system} {
        module = { kubenix, ... }: {
          imports = with kubenix.modules; [ k8s helm ];

          kubernetes = {
            helm.releases = {
              "argo-cd" = {
                namespace = "argocd";
                chart = kubenix.lib.helm.fetch {
                  repo = "https://argoproj.github.io/argo-helm";
                  chart = "argo-cd";
                  version = "7.7.10";
                  sha256 =
                    "sha256-iDc4TlZgIcFCZQ9L5RbSXcD/IlHgsgkdXYBm5y7lF48=";
                };

                values = {
                  inherit domain;
                  configs.params."server.insecure" = true;
                  server = {
                    ingress = {
                      annotations = {
                        "cert-manager.io/cluster-issuer" = "letsencrypt-prod";
                        "ingress.kubernetes.io/force-ssl-redirect" = "true";
                      };
                      enabled = true;
                      tls = true;
                      hostname = domain;
                    };
                  };
                };
              };
            };

            resources = {
              configMaps.artifact-repositories = {
                metadata.annotations."workflows.argoproj.io/default-artifact-repository" =
                  "default-v1-s3-artifact-repository";

                data.default-v1-s3-artifact-repository = ''
                  s3:
                    bucket: ${bucket-name}
                    endpoint: ${endpoint}
                    insecure: true
                    accessKeySecret:
                      name: ${secret-name}
                      key: ${access-secret-key}
                    secretKeySecret:
                      name: ${secret-name}
                      key: ${secret-secret-key}
                '';
              };

              secrets."${secret-name}" = {
                stringData = {
                  password = "ref+file://minio-auth.yaml#password";
                  user = "ref+file://minio-auth.yaml#user";
                };
              };
            };
          };
        };
      }).config.kubernetes.result;
    };
}
