{ config, secrets, ... }:
{
  services.romm = {
    enable = true;

    admin = {
      username = secrets.romm.admin.username;
      password = secrets.romm.admin.password;
    };

    authSecretKey = secrets.romm.authSecretKey;

    databaseTarget = "mariadb";
    database = {
      password = secrets.mariadb.password;
      username = "mariadb";
    };

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;

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

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides = {
      data.volumeHandle = "pvc-c41afd8b-9c13-4cff-b8a3-5f9023fb7681";
      config.volumeHandle = "pvc-79977181-2377-4be5-8218-a19774c66c15";
    };
  };
}
