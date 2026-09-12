{
  config,
  lib,
  secrets,
  ...
}:
{
  services.grafana = {
    inherit (config.devDefaults) enableLogging;
    adminPassword = secrets.grafana.password or "";
    enable = false;
    hostAffinity = "edgenix";

    ingressProvider = "traefik-lan";

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
    ]
    ++ lib.optionals config.devDefaults.enableLogging [
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
