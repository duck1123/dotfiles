{ config, secrets, ... }:
{
  services.prowlarr = {
    databaseTarget = "postgresql";
    database = {
      enable = true;
      name = "prowlarr-main";
    };

    enable = false;
    apiKey = secrets.prowlarr.key;
    hostAffinity = "edgenix";
    image = "linuxserver/prowlarr:2.6.5.5623-ls163";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Arr";

    replicas = 1;
    vpn.enable = false;

  };
}
