{ ... }:
{
  flake.nixidyApps.satisfactory =
    {
      config,
      lib,
      self,
      ...
    }:
    self.lib.mkArgoApp { inherit config lib; } {
      name = "satisfactory";

      # https://artifacthub.io/packages/helm/schichtel/satisfactory
      chart = lib.helm.downloadHelmChart {
        repo = "https://schich.tel/helm-charts";
        chart = "satisfactory";
        version = "0.3.2";
        chartHash = "sha256-CYnUMLbairir7jeeHARCX9agXfVryNTRT5uNAdSiIpM=";
      };

      defaultValues = cfg: {
        env = [
          {
            name = "DEBUG";
            value = "false";
          }
          {
            name = "STEAM_BETA";
            value = "false";
          }
        ];
        satisfactoryOpts = { };
      };
    };
}
