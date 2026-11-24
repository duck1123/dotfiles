_: {
  services.libre-closet = {
    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Organization";
  };
}
