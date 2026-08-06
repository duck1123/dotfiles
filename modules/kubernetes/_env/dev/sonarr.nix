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
    image = "linuxserver/sonarr:4.0.17.2952-ls312";
    hostAffinity = "edgenix";

    ingress = {
      domain = "sonarr.${config.devDefaults.homeDomain}";
      ingressClassName = "traefik";
      clusterIssuer = config.devDefaults.clusterIssuer;
      tls.enable = true;
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
