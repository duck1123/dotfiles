_: {
  services.nostalgiatv = {
    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Media";
  };
}
