_: {
  services.keycloak = {
    databaseTarget = "postgresql";
    ingressProvider = "traefik-dev";
    ingress.adminDomain = "keycloak-admin.dev.kronkltd.net";
  };
}
