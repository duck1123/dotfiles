{ config, secrets, ... }:
{
  services.pihole = {
    auth = { inherit (secrets.pihole) email password; };
    enable = true;
    hostAffinity = "nasnix";

    ingress = {
      domain = "pihole.${config.devDefaults.homeDomain}";
      ingressClassName = "traefik";
      clusterIssuer = config.devDefaults.clusterIssuer;
      tls.enable = true;
    };
    serviceDnsLoadBalancerIP = "192.168.0.243";
    storageClassName = "longhorn";
    # Wildcard: dev/home zone queries resolve to the Traefik LoadBalancer IP.
    # Requires clients to use Pi-hole as their DNS server.
    customDnsEntries = [
      "address=/.dev.kronkltd.net/192.168.0.242"
      "address=/.home.kronkltd.net/192.168.0.242"
    ];
  };
}
