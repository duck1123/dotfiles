{ config, secrets, ... }:
{
  services.n8n = {
    enable = false;

    hostAffinity = "nasnix";

    encryptionKey = (secrets.n8n or { }).encryptionKey or "";

    ingress = {
      domain = "n8n.${config.devDefaults.tailDomain}";
      ingressClassName = "tailscale";
      clusterIssuer = "tailscale";
    };
  };
}
