{ ... }:
{
  flake.nixidyApps.lldap =
    {
      config,
      lib,
      self,
      ...
    }:
    self.lib.mkArgoApp { inherit config lib; } {
      name = "lldap";

      chart = lib.helm.downloadHelmChart {
        repo = "https://djjudas21.github.io/charts/";
        chart = "lldap";
        version = "0.4.2";
        chartHash = "sha256-YwInTAIEIpWS/Sd4Kb4ABsH2rYGg/zcpTQGoJW8wbSQ=";
      };

      uses-ingress = true;

      extraOptions = {
      };

      defaultValues = cfg: {
        lldap.baseDN = "dc=kronkltd,dc=net";
      };
    };
}
