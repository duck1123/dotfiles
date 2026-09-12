{ ... }:
{
  flake.nixidyApps.argo-events =
    {
      config,
      lib,
      self,
      ...
    }:
    self.lib.mkArgoApp { inherit config lib; } {
      name = "argo-events";

      # https://artifacthub.io/packages/helm/argo/argo-events
      chart = lib.helm.downloadHelmChart {
        repo = "https://argoproj.github.io/argo-helm";
        chart = "argo-events";
        version = "2.4.27";
        chartHash = "sha256-Ukdoy13K2xDJ/iC2OlB4QJ1feqnpXp+Oxe0acKhGVAo=";
      };

      defaultValues = cfg: { metrics.enabled = true; };
    };
}
