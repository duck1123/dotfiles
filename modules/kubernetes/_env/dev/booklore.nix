{ config, secrets, ... }:
{
  services.booklore = {
    enable = false;
    hostAffinity = "edgenix";

    database = {
      host = "mariadb.mariadb";
      password = secrets.booklore.database.password;
      port = 3306;
      name = "booklore";
      username = "booklore";
    };

    gid = "0";

    ingress = {
      domain = "booklore.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
      clusterIssuer = "tailscale";
      # Optional: Enable local-only ingress using Traefik
      localIngress = {
        enable = true;
        domain = "booklore.local";
        tls.enable = false; # Set to true if you have cert-manager configured for local domains
      };
    };

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}/Books";
    };

    storageClassName = "longhorn";
    uid = "0";
  };
}
