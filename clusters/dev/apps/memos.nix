_: {
  services.memos = {
    # hostAffinity = "edgenix";

    databaseTarget = "postgresql";
    database.username = "postgres";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    homepage.group = "Organization";

    monitoring.autokuma.enable = true;
  };
}
