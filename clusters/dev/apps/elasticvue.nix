_: {
  services.elasticvue = {
    ingressProvider = "traefik-lan";

    clusters = [
      {
        name = "Ditto Relay (OpenSearch)";
        uri = "http://ditto-relay-opensearch.ditto-relay:9200";
      }
    ];

    homepage.group = "Database";
  };
}
