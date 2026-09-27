{ secrets, ... }:
{
  services.tailscale = {
    enable = true;
    oauth = { inherit (secrets.tailscale) authKey clientId clientSecret; };
    # Subnet routing is handled by nasnix host-level Tailscale (~/dotfiles),
    # which can reach MetalLB VIPs. The Connector pod approach doesn't work
    # because MetalLB L2 VIPs are unreachable from inside the pod overlay network.
    # subnetRoutes = [ "192.168.0.0/24" ];
  };
}
