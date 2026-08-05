{ config, ... }:
{
  services.demo = {
    enable = true;
    ingress = {
      domain = "demo.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
      clusterIssuer = "tailscale";
    };
  };
}
