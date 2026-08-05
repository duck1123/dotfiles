{ config, ... }:
{
  services.longhorn = {
    enable = true;

    ingress = {
      domain = "longhorn.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
      tls.enable = true;
    };
  };
}
