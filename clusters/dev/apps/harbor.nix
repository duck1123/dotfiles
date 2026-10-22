{ config, ... }:
{
  services.harbor = {
    domain = "harbor.${config.devDefaults.baseDomain}";
  };
}
