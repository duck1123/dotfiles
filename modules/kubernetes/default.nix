{ config, inputs, pkgs, ... }: {
  environment.systemPackages = with pkgs; [ k3s nfs-utils openiscsi ];

  networking.firewall = {
    enable = false;
    allowedTCPPorts = [ 6443 ];
    allowedUDPPorts = [ 8472 ];
  };

  services = {
    k3s = {
      enable = false;
      role = "server";
      extraFlags = toString [ "--disable=traefik" ];
    };

    openiscsi = {
      enable = true;
      name = "${config.hostname}-initiatorhost";
    };
  };
}
