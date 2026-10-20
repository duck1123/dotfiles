_: {
  services.keycloak = {
    enable = false;
    databaseTarget = "postgresql";
    ingressProvider = "traefik-dev";
    ingress.adminDomain = "keycloak-admin.dev.kronkltd.net";
  };
}
