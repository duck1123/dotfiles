{ config, secrets, ... }:
{
  services.grafana = {
    adminPassword = secrets.grafana.password or "";
    enable = true;
    hostAffinity = "edgenix";

    ingress = {
      clusterIssuer = "tailscale";
      domain = "grafana.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
      localIngress = {
        enable = true;
        domain = "grafana.${config.devDefaults.homeDomain}";
        clusterIssuer = config.devDefaults.clusterIssuer;
        tls.enable = true;
      };
    };

    additionalDatasources = [
      {
        name = "Prometheus";
        type = "prometheus";
        access = "proxy";
        url = "http://prometheus-kube-prometheus-prometheus.prometheus:9090";
        isDefault = true;
        editable = true;
        jsonData.httpMethod = "POST";
      }
      {
        name = "Loki";
        type = "loki";
        access = "proxy";
        url = "http://loki-gateway.loki.svc.cluster.local";
        editable = true;
      }
    ];

    additionalDashboardProviders = [
      {
        name = "default";
        orgId = 1;
        folder = "";
        type = "file";
        disableDeletion = false;
        editable = true;
        options.path = "/var/lib/grafana/dashboards/default";
      }
    ];
  };
}
