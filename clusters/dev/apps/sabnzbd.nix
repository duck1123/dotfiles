{ config, secrets, ... }:
{
  services.sabnzbd = {
    apiKey = secrets.sabnzbd.key;
    hostAffinity = "edgenix";

    homepage.group = "Download";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}";
    };

    useProbes = false;
  };
}
