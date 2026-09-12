{ ... }:
{
  flake.nixidyApps.argo-workflows =
    {
      config,
      lib,
      self,
      ...
    }:
    with lib;
    self.lib.mkArgoApp { inherit config lib; } {
      name = "argo-workflows";

      # https://artifacthub.io/packages/helm/argo/argo-workflows
      # (bitnami/argo-workflows was frozen/renumbered behind Bitnami's Aug
      # 2025 paid-tier restructuring; this is the official argoproj chart,
      # same repo already used for argo-events)
      chart = lib.helm.downloadHelmChart {
        repo = "https://argoproj.github.io/argo-helm";
        chart = "argo-workflows";
        version = "2.0.4";
        chartHash = "sha256-7XNDHqlTJQhVXfCbnIypbd0HTeey0Y5WqBaVi0diCDM=";
      };

      uses-ingress = true;

      defaultValues = cfg: {
        controller = {
          extraEnv = [
            {
              # https://argo-workflows.readthedocs.io/en/latest/executor_plugins/
              name = "ARGO_EXECUTOR_PLUGINS";
              value = "true";
            }
          ];

          # Workflow Archive needs a real postgres/mysql/mariadb connection
          # under controller.persistence.{postgresql,mysql} in this chart
          # (unlike bitnami's plain persistence.archive.enabled boolean) --
          # wire one up here before enabling if archiving is wanted.

          telemetryConfig.enabled = true;
        };

        server.ingress = with cfg.ingress; {
          enabled = true;
          inherit ingressClassName;
          hosts = [ domain ];
          annotations = {
            "cert-manager.io/cluster-issuer" = clusterIssuer;
            "ingress.kubernetes.io/force-ssl-redirect" = "true";
            "ingress.kubernetes.io/proxy-body-size" = "0";
            "ingress.kubernetes.io/ssl-redirect" = "true";
          };
          tls = [
            {
              hosts = [ domain ];
              secretName = "argo-workflows-tls";
            }
          ];
        };
      };

      extraResources = cfg: {
        secrets."duck.service-account-token" = {
          metadata.annotations."kubernetes.io/service-account.name" = "duck";
          type = "kubernetes.io/service-account-token";
        };
      };
    };
}
