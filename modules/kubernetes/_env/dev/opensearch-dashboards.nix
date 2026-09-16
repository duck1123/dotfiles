{ ... }:
{
  services.opensearch-dashboards = {
    enable = true;

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;

    # Points at ditto-relay's bundled single-node OpenSearch (see
    # applications/ditto-relay.nix) -- cross-namespace Service DNS.
    opensearchHosts = [ "http://ditto-relay-opensearch.ditto-relay:9200" ];

    homepage.group = "Database";
  };
}
