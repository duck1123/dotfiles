_: {
  services.navidrome = {
    enable = false;

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    monitoring.autokuma.enable = true;

    nfsTarget = "nas";
    nfsSubPath = "Music";
    nfs.enable = true;

  };
}
