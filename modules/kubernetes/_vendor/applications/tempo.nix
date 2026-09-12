{ ... }:
{
  flake.nixidyApps.tempo =
    {
      charts,
      config,
      lib,
      self,
      ...
    }:
    with lib;
    self.lib.mkArgoApp { inherit config lib; } {
      name = "tempo";

      # https://artifacthub.io/packages/helm/grafana/tempo
      chart = charts.grafana.tempo;

      uses-ingress = true;

      defaultValues = cfg: {
        persistence = {
          enabled = true;
          storageClassName = cfg.storageClassName;
        };

        tempo.retention = "72h";

        tempoQuery = {
          enabled = true;
          tag = "latest";
          ingress = with cfg.ingress; {
            inherit ingressClassName;
            enabled = true;
            # annotations = {
            #   "cert-manager.io/cluster-issuer" = clusterIssuer;
            #   "ingress.kubernetes.io/force-ssl-redirect" = "true";
            #   "ingress.kubernetes.io/proxy-body-size" = "0";
            #   "ingress.kubernetes.io/ssl-redirect" = "true";
            #   "traefik.ingress.kubernetes.io/router.entrypoints" = "websecure";
            #   "traefik.ingress.kubernetes.io/router.middlewares" =
            #     "authentik-middlewares-authenkik@kubernetescrd";
            # };
            hosts = [ domain ];
            tls = [
              {
                secretName = "tempo-tls";
                hosts = [ domain ];
              }
            ];
          };
        };
      };
    };
}
