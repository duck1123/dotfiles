{ config, identity, inputs, pkgs, ... }: {
  environment.systemPackages = with pkgs; [ k3s nfs-utils openiscsi ];

  networking.firewall = {
    enable = false;
    allowedTCPPorts = [ 6443 ];
    allowedUDPPorts = [ 8472 ];
  };

  services = {
    k3s = {
      enable = true;
      role = "server";
      extraFlags =
        toString [ "--cluster-cidr=10.42.0.0/16" "--disable=traefik" ];
    };

    openiscsi = {
      enable = true;
      name = "${identity.hostname}-initiatorhost";
    };
  };
}
