{ config, secrets, ... }:
{
  services.prowlarr = {
    database = {
      enable = false;
      host = "postgresql.postgresql";
      port = 5432;
      name = "prowlarr-main";
      username = "prowlarr";
      password = secrets.postgresql.userPassword;
    };

    enable = true;
    hostAffinity = "edgenix";

    ingress = {
      domain = "prowlarr.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
      clusterIssuer = "tailscale";
    };

    replicas = 1;
    vpn.enable = false;
  };
}
