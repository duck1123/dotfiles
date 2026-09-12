{ ... }:
{
  flake.nixidyApps.mssql =
    {
      config,
      lib,
      self,
      ...
    }:
    self.lib.mkArgoApp { inherit config lib; } {
      name = "mssql";

      chart = lib.helm.downloadHelmChart {
        repo = "https://simcubeltd.github.io/simcube-helm-charts/";
        chart = "mssqlserver-2022";
        version = "1.2.3";
        chartHash = "sha256-IdqGRmO6dAeupsqtT7YVqE080GRC8kYL5aM7keV8JTk=";
      };

      defaultValues = cfg: { acceptEula.value = "yes"; };
    };
}
