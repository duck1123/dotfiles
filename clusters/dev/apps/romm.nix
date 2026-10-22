{ config, secrets, ... }:
{
  services.romm = {
    admin = {
      username = secrets.romm.admin.username;
      password = secrets.romm.admin.password;
    };

    authSecretKey = secrets.romm.authSecretKey;

    databaseTarget = "mariadb";

    # Off: RomM runs its own valkey in the pod. With the shared redis it uses
    # redis.dbIndex (7) so its RQ queues don't collide with other apps.
    # redisTarget = "redis";
    database = {
      password = secrets.mariadb.password;
      username = "mariadb";
    };

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Media";

    metadata.igdb = {
      enable = true;
      clientId = secrets.romm.metadata.igdb.clientId;
      clientSecret = secrets.romm.metadata.igdb.clientSecret;
    };

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      libraryPath = "${config.devDefaults.nasBase}/Roms";
      assetsPath = "${config.devDefaults.nasBase}/Roms/assets";
      resourcesPath = "${config.devDefaults.nasBase}/Roms/resources";
    };
  };
}
