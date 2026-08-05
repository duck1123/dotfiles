{ config, secrets, ... }:
{
  services.sonarr = {
    database = {
      enable = true;
      host = "postgresql.postgresql";
      port = 5432;
      name = "sonarr";
      username = "sonarr";
      password = secrets.postgresql.userPassword;
    };

    enable = true;
    image = "linuxserver/sonarr:version-4.0.17.2952";
    hostAffinity = "edgenix";

    ingress = {
      domain = "sonarr.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
      clusterIssuer = "tailscale";
    };

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}";
    };

    replicas = 1;
    vpn.enable = false;
  };
}
