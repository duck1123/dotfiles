{ ... }:
{
  services.elasticvue = {
    enable = false;

    ingressProvider = "traefik-lan";

    # tube-archivist's Elasticsearch entry removed -- tube-archivist is
    # disabled (see env/dev/tube-archivist.nix), so that backend no longer
    # exists. Add it back if tube-archivist ever gets a working fix.
    clusters = [
      {
        name = "Ditto Relay (OpenSearch)";
        uri = "http://ditto-relay-opensearch.ditto-relay:9200";
      }
    ];

    homepage.group = "Database";
  };
}
