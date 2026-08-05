{ ... }:
{
  services.loki = {
    enable = true;
    hostAffinity = "edgenix";
    retention = "720h"; # 30 days
    storageClassName = "longhorn";
    storageSize = "20Gi";
  };
}
