_: {
  services.gramps-web = {
    redisTarget = "redis";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Organization";
  };
}
