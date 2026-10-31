_: {
  services.tautulli = {
    image = "linuxserver/tautulli:v2.18.2-ls250";
    # Next to Plex (NixOS `services.plex` on edgenix, 192.168.0.22:32400),
    # which Tautulli is pointed at in its setup wizard.
    hostAffinity = "edgenix";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Media";
  };
}
