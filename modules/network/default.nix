{ config, pkgs, ... }: {
  # Enable network discovery and file sharing services
  services = {
    # GVFS - Virtual filesystem layer for GNOME
    gvfs.enable = true;

    # Avahi - Network service discovery (mDNS)
    avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
    };

    # Samba client for Windows/SMB shares
  };

  # Add necessary packages for network access
  environment.systemPackages = with pkgs; [ gvfs nfs-utils cifs-utils ];

  # Open necessary firewall ports for network discovery
  networking.firewall = {
    allowedUDPPorts = [
      5353 # mDNS
      137 # NetBIOS
      138 # NetBIOS
    ];
    allowedTCPPorts = [
      139 # NetBIOS
      445 # SMB
    ];
  };
}

