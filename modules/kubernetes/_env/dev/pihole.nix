{ config, secrets, ... }:
{
  services.pihole = {
    auth = { inherit (secrets.pihole) email password; };
    enable = true;
    hostAffinity = "nasnix";

    ingress = {
      domain = "pihole.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
      clusterIssuer = "tailscale";
      tls.enable = true;
    };

    ingress.localIngress = {
      enable = true;
      serviceName = "pihole-web";
      servicePort = 80;
    };
    serviceDnsLoadBalancerIP = "192.168.0.242";
    storageClassName = "longhorn";
    # Wildcard: all *.local queries resolve to the Traefik LoadBalancer IP.
    # Requires clients to use Pi-hole as their DNS server.
    customDnsEntries = [
      "address=/.local/192.168.0.241"
      "address=/.dev.kronkltd.net/192.168.0.241"
    ];
  };
}
