{ secrets, ... }:
{
  services.sonarr = {
    databaseTarget = "postgresql";
    database.enable = true;

    enable = false;
    apiKey = secrets.sonarr.key;
    image = "linuxserver/sonarr:4.0.20.3014-ls327";
    hostAffinity = "edgenix";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Arr";

    nfsTarget = "nas";
    nfs.enable = true;

    replicas = 1;
    vpn.enable = false;

  };
}
