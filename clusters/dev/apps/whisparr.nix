{ config, ... }:
{
  services.whisparr = {
    databaseTarget = "postgresql";
    database.enable = true;

    enable = false;

    ingressProvider = "traefik-lan";

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}";
    };

    replicas = 1;

  };
}
