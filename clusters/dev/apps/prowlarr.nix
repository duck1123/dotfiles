{ config, secrets, ... }:
{
  services.prowlarr = {
    databaseTarget = "postgresql";
    database = {
      enable = true;
      name = "prowlarr-main";
    };

    apiKey = secrets.prowlarr.key;
    hostAffinity = "edgenix";
    image = "linuxserver/prowlarr:2.6.5.5623-ls163";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Arr";

    vpn.enable = false;
  };
}
