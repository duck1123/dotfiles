_: {
  services.traefik = {
    enable = true;
    service.loadBalancerIP = "192.168.0.242";
    service.hostPorts = false;
  };
}
