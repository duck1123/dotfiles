{ config, secrets, ... }:
{
  services.pihole = {
    auth = { inherit (secrets.pihole) email password; };
    enable = false;
    hostAffinity = "nasnix";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    # Root "/" doesn't return a monitorable response; the pod's own liveness/
    # readiness probes already settled on /admin for the same reason (see
    # applications/pihole.nix). Point straight at /admin/login rather than /admin
    # so this doesn't depend on Uptime Kuma following pihole's redirect.
    monitoring.autokuma.url = "https://${config.services.pihole.ingress.domain}/admin/login";
    serviceDnsLoadBalancerIP = "192.168.0.243";
    storageClassName = "longhorn";
    customDnsEntries = [
      "address=/.dev.kronkltd.net/192.168.0.242"
      "address=/.home.kronkltd.net/192.168.0.242"
    ];

    # Captured via `kubectl get pv <name> -o jsonpath='{.spec.csi.volumeHandle}'`
    # -- see docs/pinned-volumes.md. Specific to this cluster.
    volumeOverrides.data.volumeHandle = "pvc-131cfa45-b850-44ac-a9b1-81fadbc4bdd2";
  };
}
