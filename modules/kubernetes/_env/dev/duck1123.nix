{ ... }:
{
  # pubkey/relays/NIP-05 are hardcoded in applications/duck1123-site/ (a real
  # npm/Vite project, not per-env Nix options) since this is a single-identity
  # site — see applications/duck1123-site/src/config.js and
  # public/.well-known/nostr.json to change them.
  services.duck1123 = {
    enable = true;

    # duck1123 has no k8s Ingress (it's reached via the Cloudflare Tunnel, see
    # applications/duck1123/default.nix), so homepage/monitoring.autokuma have
    # no ingress domain to default their href/url to — set both explicitly.
    # Grouped with ditto-relay under the "Nostr" homepage heading (see
    # env/dev.nix's homepageGroups).
    homepage = {
      enable = true;
      group = "Nostr";
      href = "https://duck1123.com";
    };

    monitoring.autokuma = {
      enable = true;
      url = "https://duck1123.com";
    };
  };
}
