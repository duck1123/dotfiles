{ config, secrets, ... }:
{
  services.authentik = {
    inherit (secrets.authentik) secret-key;
    enable = false;

    ingressProvider = "traefik-dev";

    postgresql = {
      inherit (secrets.authentik.postgresql)
        password
        postgres-password
        replicationPassword
        ;
      host = "postgreql.postgreql";
    };
  };
}
