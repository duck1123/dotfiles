{ ... }:
{
  flake.nixidyApps.kyverno =
    {
      config,
      lib,
      self,
      ...
    }:
    self.lib.mkArgoApp { inherit config lib; } {
      name = "kyverno";

      # https://artifacthub.io/packages/helm/kyverno/kyverno
      chart = lib.helm.downloadHelmChart {
        repo = "https://kyverno.github.io/kyverno/";
        chart = "kyverno";
        version = "3.4.4";
        chartHash = "sha256-3GVeYp+xvr8LKeIVVqqsSBwDcr7KR4yFBLuBKCXHo1s=";
      };

      defaultValues = cfg: { };
    };
}
