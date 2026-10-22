_: {
  services.gramps-web = {
    enable = false;

    redisTarget = "redis";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Organization";
  };
}
