{ ... }:
{
  services.crafty-controller = {
    enable = false;

    serviceType = "LoadBalancer";
    storageClassName = "longhorn";
    # loadBalancerIP = "192.168.0.244";

    # Widen serverPortRange here (default 25565-25570) if more than 6
    # concurrent Minecraft servers are needed.
  };
}
