{ config, secrets, ... }:
{
  services.pihole = {
    auth = { inherit (secrets.pihole) email password; };
    hostAffinity = "nasnix";

    ingressProvider = "traefik-lan";
    ingress.tls.enable = true;
    # Root "/" doesn't return a monitorable response; the pod's own liveness/
    # readiness probes already settled on /admin for the same reason (see
    # appTemplates/pihole.nix). Point straight at /admin/login rather than /admin
    # so this doesn't depend on Uptime Kuma following pihole's redirect.
    monitoring.autokuma.url = "https://${config.services.pihole.ingress.domain}/admin/login";
    serviceDnsLoadBalancerIP = "192.168.0.243";
    storageClassName = "longhorn";
    customDnsEntries = [
      "address=/.dev.kronkltd.net/192.168.0.242"
      "address=/.home.kronkltd.net/192.168.0.242"
    ];
  };
}
