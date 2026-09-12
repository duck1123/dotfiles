{ config, ... }:
{
  services.argo-workflows = {
    enable = false;

    ingressProvider = "traefik-dev";
  };
}
