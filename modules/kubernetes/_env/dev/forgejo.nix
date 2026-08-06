{ config, secrets, ... }:
{
  services.forgejo = {
    admin = { inherit (secrets.forgejo.admin) password username; };
    enable = true;

    ingress = {
      domain = "forgejo.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
      localIngress = {
        enable = true;
        domain = "forgejo.${config.devDefaults.homeDomain}";
        clusterIssuer = config.devDefaults.clusterIssuer;
        tls.enable = true;
      };
    };

    postgresql = {
      inherit (secrets.forgejo.postgresql)
        adminPassword
        adminUsername
        replicationPassword
        userPassword
        ;
    };

    storageClassName = "longhorn";
  };
}
