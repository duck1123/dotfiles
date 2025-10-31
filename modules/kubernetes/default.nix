{ host, lib, pkgs, ... }: {
  options = {
    features.kubernetes.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable kubernetes";
    };
  };

  config = lib.mkIf host.features.kubernetes.server.enable {
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
        name = "${host.hostname}-initiatorhost";
      };
    };

    system.activationScripts.makeUsrBinSymlinks = ''
      mkdir -p /usr/bin
      ln -sf ${pkgs.openiscsi}/bin/iscsiadm /usr/bin/iscsiadm
    '';
  };
}
