{ ... }:
{
  flake.nixidyApps.loki =
    {
      charts,
      config,
      lib,
      self,
      ...
    }:
    with lib;
    self.lib.mkArgoApp { inherit config lib; } {
      name = "loki";

      # https://artifacthub.io/packages/helm/grafana/loki
      chart = charts.grafana.loki;

      extraOptions = {
        enableLogging = mkOption {
          description = mdDoc "Enable Loki log aggregation service";
          type = types.bool;
          default = true;
        };

        retention = mkOption {
          description = mdDoc "Log retention period (e.g. '720h' for 30 days, '168h' for 7 days)";
          type = types.str;
          default = "720h";
        };

        storageSize = mkOption {
          description = mdDoc "Size of the Loki data PVC";
          type = types.str;
          default = "20Gi";
        };
      };

      defaultValues = cfg: {
        loki = {
          auth_enabled = false;

          commonConfig.replication_factor = 1;

          storage.type = "filesystem";

          schemaConfig.configs = [
            {
              from = "2024-01-01";
              store = "tsdb";
              object_store = "filesystem";
              schema = "v13";
              index = {
                prefix = "loki_index_";
                period = "24h";
              };
            }
          ];

          limits_config.retention_period = cfg.retention;

          compactor = {
            retention_enabled = true;
            working_directory = "/var/loki/retention";
            delete_request_store = "filesystem";
          };
        };

        deploymentMode = "SingleBinary";

        singleBinary = {
          replicas = 1;
          persistence = {
            enabled = true;
            size = cfg.storageSize;
            storageClass = cfg.storageClassName;
          };
        };

        # Disable distributed components not used in SingleBinary mode
        backend.replicas = 0;
        read.replicas = 0;
        write.replicas = 0;

        # Gateway exposes Loki's push/query API for Promtail and Grafana
        gateway.enabled = true;

        # Caches are not useful in single-binary mode
        chunksCache.enabled = false;
        resultsCache.enabled = false;
      };
    };
}
