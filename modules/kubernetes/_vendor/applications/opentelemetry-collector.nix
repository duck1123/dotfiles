{ ... }:
{
  flake.nixidyApps.opentelemetry-collector =
    {
      config,
      lib,
      ...
    }:
    let
      cfg = config.services.opentelemetry-collector;

      chart = lib.helm.downloadHelmChart {
        repo = "https://open-telemetry.github.io/opentelemetry-helm-charts";
        chart = "opentelemetry-collector";
        version = "0.107.0";
        chartHash = "sha256-MVHgArh62dp58Y9TzJ2sEhgBkBHntw+6/u/pcejqIAA=";
      };

      defaultNamespace = "opentelemetry-collector";

      defaultValues = {
        mode = "deployment";
        presets.logsCollection.enabled = false;
        image.repository = "otel/opentelemetry-collector-contrib";
        config = {
          receivers.otlp.protocols = {
            grpc.endpoint = "0.0.0.0:4317";
            http.endpoint = "0.0.0.0:4318";
          };
          processors = {
            memory_limiter = {
              check_interval = "1s";
              limit_mib = 512;
            };
            batch = { };
          };

          connectors = {
            spanmetrics = {
              namespace = "traces.spanmetrics";
              metrics_flush_interval = "15s";
              dimensions = [ { name = "http.response.status_code"; } ];
              exemplars.enabled = true;
              histogram = {
                unit = "s";
                explicit.buckets = [
                  "5ms"
                  "10ms"
                  "25ms"
                  "50ms"
                  "75ms"
                  "100ms"
                  "250ms"
                  "500ms"
                  "750ms"
                  "1s"
                  "2.5s"
                  "5s"
                  "7.5s"
                  "10s"
                ];
              };
            };
            servicegraph = {
              latency_histogram_buckets = [
                "5ms"
                "10ms"
                "25ms"
                "50ms"
                "75ms"
                "100ms"
                "250ms"
                "500ms"
                "750ms"
                "1s"
                "2.5s"
                "5s"
                "7.5s"
                "10s"
              ];
              store = {
                ttl = "15s";
                max_items = 500;
              };

              metrics_flush_interval = "15s";
            };
          };

          exporters = {
            debug = { };
            "otlphttp/loki" = {
              endpoint = "http://loki-gateway/otlp";
              tls.insecure = true;
            };
            "otlp/tempo" = {
              endpoint = "tempo:4317";
              tls.insecure = true;
            };
            prometheusremotewrite = {
              endpoint = "http://prometheus-kube-prometheus-prometheus.prometheus:9090/api/v1/write";
              tls.insecure = true;
            };
          };

          extensions.health_check = { };

          service = {
            extensions = [ "health_check" ];
            pipelines = {
              logs = {
                receivers = [ "otlp" ];
                processors = [
                  "memory_limiter"
                  "batch"
                ];
                exporters = [
                  "debug"
                  "otlphttp/loki"
                ];
              };
              traces = {
                receivers = [ "otlp" ];
                processors = [
                  "memory_limiter"
                  "batch"
                ];
                exporters = [
                  "debug"
                  "otlp/tempo"
                  "spanmetrics"
                  "servicegraph"
                ];

              };
              metrics = {
                receivers = [
                  "otlp"
                  "spanmetrics"
                  "servicegraph"
                ];
                processors = [
                  "memory_limiter"
                  "batch"
                ];
                exporters = [
                  "debug"
                  "prometheusremotewrite"
                ];
              };
            };
          };
        };
      };

      values = lib.attrsets.recursiveUpdate defaultValues cfg.values;
      namespace = cfg.namespace;
    in
    with lib;
    {
      options.services.opentelemetry-collector = {
        enable = mkEnableOption "Enable application";
        namespace = mkOption {
          description = mdDoc "The namespace to install into";
          type = types.str;
          default = defaultNamespace;
        };

        values = mkOption {
          description = "All the values";
          type = types.attrsOf types.anything;
          default = { };
        };
      };

      config = mkIf cfg.enable {
        applications.opentelemetry-collector = {
          inherit namespace;
          createNamespace = true;
          finalizer = "foreground";
          helm.releases.opentelemetry-collector = { inherit chart values; };
          syncPolicy.finalSyncOpts = [ "CreateNamespace=true" ];
        };
      };
    };
}
