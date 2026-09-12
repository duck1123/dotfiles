{ ... }:
{
  services.memos = {
    enable = false;
    # hostAffinity = "edgenix";

    databaseTarget = "postgresql";
    database.username = "postgres";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Notes";

    monitoring.autokuma.enable = true;
  };
}
