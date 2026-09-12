{ config, ... }:
{
  services.loki = {
    inherit (config.devDefaults) enableLogging;
    enable = config.devDefaults.enableLogging;
    hostAffinity = "edgenix";
    retention = "720h"; # 30 days
    storageClassName = "longhorn";
    storageSize = "20Gi";
  };
}
