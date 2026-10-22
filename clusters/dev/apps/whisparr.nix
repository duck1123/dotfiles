{ config, ... }:
{
  services.whisparr = {
    databaseTarget = "postgresql";
    database.enable = true;

    ingressProvider = "traefik-lan";

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}";
    };
  };
}
