{ config, ... }:
{
  services.promtail = {
    inherit (config.devDefaults) enableLogging;
    enable = config.devDefaults.enableLogging;
  };
}
