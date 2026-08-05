{ config, secrets, ... }:
{
  services.whisparr = {
    database = {
      enable = true;
      host = "postgresql.postgresql";
      port = 5432;
      name = "whisparr";
      username = "whisparr";
      password = secrets.postgresql.userPassword;
    };

    enable = false;

    ingress = {
      domain = "whisparr.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
      clusterIssuer = "tailscale";
    };

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}";
    };

    replicas = 1;
  };
}
