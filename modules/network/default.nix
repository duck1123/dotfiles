{ pkgs, ... }: {
  environment.systemPackages = with pkgs; [ gvfs nfs-utils cifs-utils ];

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

  services = {
    gvfs.enable = true;

    avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
    };
  };
}

