_: {
  services.opensearch-dashboards = {
    enable = false;

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;

    # Points at ditto-relay's bundled single-node OpenSearch (see
    # applications/ditto-relay.nix) -- cross-namespace Service DNS.
    opensearchHosts = [ "http://ditto-relay-opensearch.ditto-relay:9200" ];

    # Without this, ditto-relay's `nostr-events` index has data in it but
    # nothing shows up in Discover -- Dashboards needs an Index Pattern saved
    # object before it'll query an index, and there's no UI-less way to see
    # that one's missing. No timeFieldName: `created_at` is stored as a Nostr
    # (unix-seconds) `long`, and Dashboards' time filter treats numeric time
    # fields as milliseconds-since-epoch, which would render every doc as
    # some date in the far future.
    indexPatterns = [ { title = "nostr-events"; } ];

    homepage.group = "Database";
  };
}
