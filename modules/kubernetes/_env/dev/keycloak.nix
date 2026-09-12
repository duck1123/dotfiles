{ ... }:
{
  services.keycloak = {
    enable = false;
    ingressProvider = "traefik-dev";
    ingress.adminDomain = "keycloak-admin.dev.kronkltd.net";
  };
}
