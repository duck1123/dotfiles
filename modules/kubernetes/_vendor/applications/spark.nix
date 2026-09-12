{ ... }:
{
  flake.nixidyApps.spark =
    {
      config,
      lib,
      self,
      ...
    }:
    with lib;
    self.lib.mkArgoApp { inherit config lib; } {
      name = "spark";

      # https://artifacthub.io/packages/helm/bitnami/spark
      chart = lib.helm.downloadHelmChart {
        repo = "oci://registry-1.docker.io/bitnamicharts";
        chart = "spark";
        version = "9.3.5";
        chartHash = "sha256-Cgyer2tZyOW8oW4NltpdjlcNmbJXMcJhfNf8mBfW68s=";
      };

      uses-ingress = true;

      defaultValues =
        cfg: with cfg.ingress; {
          ingress = {
            inherit ingressClassName;
            annotations = {
              "cert-manager.io/cluster-issuer" = clusterIssuer;
              "ingress.kubernetes.io/force-ssl-redirect" = "true";
            };
            enabled = true;
            hostname = domain;
            tls = tls.enable;
          };
        };
    };
}
