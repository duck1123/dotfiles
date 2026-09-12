{ ... }:
{
  flake.nixidyApps.argocd =
    {
      config,
      lib,
      self,
      ...
    }:
    self.lib.mkArgoApp { inherit config lib; } {
      name = "argocd";

      extraResources = cfg: {
        configMaps.argocd-cm = {
          metadata.labels = {
            "app.kubernetes.io/name" = "argocd-cm";
            "app.kubernetes.io/part-of" = "argocd";
          };
          data = {
            "exec.enabled" = "true";
            "exec.shells" = "bash,sh";
            "kustomize.buildOptions" = "--enable-helm";
            "ui.bannercontent" = "Ignore This Notice!";
            "ui.bannerurl" = "https://duck1123.com/";
            "url" = "https://argocd.dev.kronkltd.net";
          };
        };
        secrets = {
          bitnamicharts = {
            metadata.labels."argocd.argoproj.io/secret-type" = "repository";
            stringData = {
              enableOCI = "true";
              name = "bitnamicharts";
              type = "helm";
              url = "registry-1.docker.io/bitnamicharts";
            };
          };
          forgejo-helm-oci = {
            metadata.labels."argocd.argoproj.io/secret-type" = "repository";
            stringData = {
              enableOCI = "true";
              name = "forgejo-helm";
              type = "helm";
              url = "registry-1.docker.io/bitnamicharts";
            };
          };
        };
      };
    };
}
