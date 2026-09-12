{ ... }:
{
  flake.nixidyApps.calibre =
    {
      config,
      lib,
      self,
      ...
    }:
    with lib;
    self.lib.mkArgoApp { inherit config lib; } {
      name = "calibre";

      # https://artifacthub.io/packages/helm/geek-cookbook/calibre
      chart = helm.downloadHelmChart {
        repo = "https://geek-cookbook.github.io/charts/";
        chart = "calibre-web";
        version = "8.4.2";
        chartHash = "sha256-TZ7mv+/blv3XY0NneSPY5u9QxfXyOxogEwAFUPH3dQU=";
      };

      uses-ingress = true;

      # https://github.com/k8s-at-home/library-charts/blob/main/charts/stable/common/values.yaml
      defaultValues = cfg: {
        ingress.main = with cfg.ingress; {
          inherit ingressClassName;

          enabled = true;
          hosts = [
            {
              host = domain;
              paths = [
                {
                  path = "/";
                  pathType = "ImplementationSpecific";
                }
              ];

            }
          ];
          tls = [ { hosts = [ domain ]; } ];
        };

        persistence = {
          books = {
            enabled = true;
            storageClass = cfg.storageClassName;
            accessMode = "ReadWriteOnce";
            size = "1Gi";
          };

          config = {
            enabled = true;
            storageClass = cfg.storageClassName;
            accessMode = "ReadWriteOnce";
          };
        };
      };
    };
}
