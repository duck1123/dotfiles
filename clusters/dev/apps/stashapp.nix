{ config, secrets, ... }:
{
  services.stashapp = {
    apiKey = secrets.stashapp.key;
    hostAffinity = "nixmini";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Media";

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}/Videos";
    };

    enableGPU = true;
  };
}
