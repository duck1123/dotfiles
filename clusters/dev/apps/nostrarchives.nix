{ config, secrets, ... }:
{
  services.nostrarchives = {
    enable = false;

    databaseTarget = "postgresql";

    redis = {
      host = "redis.redis";
      port = 6379;
      password = secrets.redis.password;
    };

    ingressProvider = "traefik-lan";

    relayDomain = "nostrarchives-relay.${config.devDefaults.homeDomain}";
  };
}
