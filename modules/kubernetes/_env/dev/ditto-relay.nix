{ secrets, ... }:
{
  services.ditto-relay = {
    enable = true;

    relayUrl = "wss://relay.duck1123.com/";
    nsec = secrets.ditto-relay.nsec;

    # Captured via `kubectl get pvc ditto-relay-ditto-relay-opensearch-data -n
    # ditto-relay -o jsonpath='{.spec.volumeName}'` then `kubectl get pv
    # <that> -o jsonpath='{.spec.csi.volumeHandle}'` -- see docs/pinned-volumes.md.
    volumeOverrides.opensearch-data.volumeHandle = "pvc-5baee1d1-efe8-464b-ae93-910d4c7c593d";

    # No k8s Ingress (reached via the Cloudflare Tunnel, see
    # applications/ditto-relay.nix and applications/cloudflared.nix), so
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
