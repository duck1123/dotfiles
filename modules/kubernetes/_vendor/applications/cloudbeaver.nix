{ ... }:
{
  flake.nixidyApps.cloudbeaver =
    {
      config,
      lib,
      self,
      ...
    }:
    with lib;
    self.lib.mkArgoApp { inherit config lib self; } {
      name = "cloudbeaver";

      # https://artifacthub.io/packages/helm/avisto/cloudbeaver
      chart = helm.downloadHelmChart {
        repo = "https://avistotelecom.github.io/charts/";
        chart = "cloudbeaver";
        version = "1.1.7";
        chartHash = "sha256-5zv8MhPH90JSo8yAQTwOgny6W3VOtBlqKgpI5hceRzQ=";
      };

      uses-ingress = true;

      # `pvcName = "cloudbeaver"` to match the chart's `existingClaim` value
      # below. Shape only -- no volumeHandle here, that's environment-specific
      # (see env/dev/cloudbeaver.nix and docs/pinned-volumes.md).
      volumes = cfg: {
        data = {
          pvcName = "cloudbeaver";
          size = "5Gi";
        };
      };

      defaultValues = cfg: {
        ingress = with cfg.ingress; {
          inherit ingressClassName;

          annotations = {
            "cert-manager.io/cluster-issuer" = clusterIssuer;
            "ingress.kubernetes.io/force-ssl-redirect" = "true";
          };

          enabled = true;
          hostname = domain;
          tls = true;
        };

        nodeSelector."kubernetes.io/hostname" = cfg.hostAffinity;

        persistence = {
          enabled = true;
          storageClass = cfg.storageClassName;
          # Pinned via the `volumes` parameter above instead of letting the
          # chart dynamically provision a fresh one -- see docs/pinned-volumes.md.
          existingClaim = "cloudbeaver";
        };
      };
    };
}
