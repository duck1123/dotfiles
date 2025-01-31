{ config, inputs, pkgs, ... }: {
  environment.systemPackages = with pkgs; [ k3s nfs-utils ];

  networking = {
    firewall = {
      enable = false;
      allowedTCPPorts = [ 6443 ];
      allowedUDPPorts = [ 8472 ];
    };
  };

  services = {
    k3s = {
      # enable = true;
      enable = false;
      role = "server";
      extraFlags = toString [ "--disable=traefik" ];
    };
  };
}
