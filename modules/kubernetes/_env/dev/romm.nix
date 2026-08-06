{ config, secrets, ... }:
{
  services.romm = {
    enable = true;

    admin = {
      username = secrets.romm.admin.username;
      password = secrets.romm.admin.password;
    };

    authSecretKey = secrets.romm.authSecretKey;

    database = {
      host = "mariadb.mariadb";
      name = "romm";
      password = secrets.mariadb.password;
      port = 3306;
      username = "mariadb";
    };

    ingress = {
      domain = "romm.${config.devDefaults.homeDomain}";
      ingressClassName = "traefik";
      clusterIssuer = config.devDefaults.clusterIssuer;
      tls.enable = true;
    };

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
