{ host, lib, pkgs, ... }: {
  config = lib.mkIf host.features.network.enable {
    environment.systemPackages = with pkgs; [ gvfs nfs-utils cifs-utils ];

    networking = {
      firewall = {
        allowedTCPPorts = [
          139 # NetBIOS
          445 # SMB
          24800 # barrier port
          32400 # Plex Media Server
        ];

        allowedUDPPorts = [
          5353 # mDNS
          137 # NetBIOS
          138 # NetBIOS
        ];

        enable = false;
      };

      hostName = host.hostname;
      networkmanager.enable = true;
    };

    services = {
      gvfs.enable = true;

      avahi = {
        enable = true;
        nssmdns4 = true;
        openFirewall = true;
      };
    };
  };
}
