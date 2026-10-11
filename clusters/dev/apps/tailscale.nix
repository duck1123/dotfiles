{ secrets, ... }:
{
  services.tailscale = {
    enable = true;
    oauth = { inherit (secrets.tailscale) authKey clientId clientSecret; };
    # The operator may stay on nasnix, which is tainted to keep ordinary
    # workloads off it. It's a chart workload, so allowControlPlane can't reach it.
    values.operatorConfig.tolerations = [
      {
        key = "node-role.kubernetes.io/control-plane";
        operator = "Exists";
      }
    ];
    # Subnet routing is handled by nasnix host-level Tailscale (~/dotfiles),
    # which can reach MetalLB VIPs. The Connector pod approach doesn't work
    # because MetalLB L2 VIPs are unreachable from inside the pod overlay network.
    # subnetRoutes = [ "192.168.0.0/24" ];
  };
}
