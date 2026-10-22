{ secrets, ... }:
{
  services.ditto-relay = {
    relayUrl = "wss://relay.duck1123.com/";
    nsec = secrets.ditto-relay.nsec;

    # No k8s Ingress (reached via the Cloudflare Tunnel, see
    # appTemplates/ditto-relay.nix and appTemplates/cloudflared.nix), so
    # homepage/monitoring.autokuma have no ingress domain to default their
    # url/href to -- set both explicitly. Grouped with duck1123 under the
    # "Nostr" homepage heading (see env/dev.nix's homepageGroups).
    homepage = {
      enable = true;
      group = "Nostr";
      displayName = "Ditto Relay";
      href = "https://relay.duck1123.com";
    };

    monitoring.autokuma = {
      enable = true;
      url = "https://relay.duck1123.com";
    };
  };
}
