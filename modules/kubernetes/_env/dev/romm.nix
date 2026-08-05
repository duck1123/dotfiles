{ config, secrets, ... }:
{
  services.romm = {
    enable = false;

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
      domain = "romm.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
      clusterIssuer = "tailscale";
      # Optional: Enable local-only ingress using Traefik
      localIngress = {
        enable = true;
        domain = "romm.local";
        tls.enable = false; # Set to true if you have cert-manager configured for local domains
      };
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
