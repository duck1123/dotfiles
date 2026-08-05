{ config, secrets, ... }:
{
  services.memos = {
    enable = true;
    hostAffinity = "edgenix";

    database = {
      host = "postgresql.postgresql";
      port = 5432;
      name = "memos";
      username = "postgres";
      password = secrets.postgresql.userPassword;
    };

    ingress = {
      domain = "memos.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
    };
  };
}
