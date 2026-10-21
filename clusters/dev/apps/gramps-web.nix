_: {
  services.gramps-web = {
    enable = true;

    redisTarget = "redis";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Organization";
  };
}
