{ config, ... }:
{
  services.komga = {
    enable = true;

    ingress = {
      domain = "komga.${config.devDefaults.tailDomain}";
      clusterIssuer = "tailscale";
      ingressClassName = "tailscale";
      localIngress.enable = true;
      tls.enable = true;
    };

    nfs = {
      enable = true;
      server = config.devDefaults.nasHost;
      path = "${config.devDefaults.nasBase}/Books";
    };
  };
}
