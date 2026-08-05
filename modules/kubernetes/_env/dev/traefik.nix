{ ... }:
{
  services.traefik = {
    enable = true;
    service.loadBalancerIP = "192.168.0.241";
    service.hostPorts = false;
  };
}
