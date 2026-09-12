{ config, ... }:
{
  services.marquez = {
    domain = "marquez.${config.devDefaults.baseDomain}";
    enable = false;
  };
}
