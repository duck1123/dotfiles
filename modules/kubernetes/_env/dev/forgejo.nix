{ config, secrets, ... }:
{
  services.forgejo = {
    admin = { inherit (secrets.forgejo.admin) password username; };
    enable = true;

    ingress = {
      domain = "forgejo.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
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
