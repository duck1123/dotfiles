{ config, ... }:
{
  services.nostrarchives = {
    enable = false;

    databaseTarget = "postgresql";

    redisTarget = "redis";

    ingressProvider = "traefik-lan";

    relayDomain = "nostrarchives-relay.${config.devDefaults.homeDomain}";
  };
}
