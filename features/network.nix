{
  homeManager =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [ networkmanager ];
    };

  nixos =
    { config, pkgs, ... }:
    {
      environment.systemPackages = with pkgs; [
        gvfs
        nfs-utils
        cifs-utils
      ];

      networking = {
        firewall = {
          # NetBIOS
          allowedTCPPorts = [ 139 ];

          allowedUDPPorts = [
            5353 # mDNS
            137 # NetBIOS
            138 # NetBIOS
          ];

          enable = false;
        };

        hostName = config.host.hostname;
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

      users.users.${config.host.identity.username}.extraGroups = [ "networkmanager" ];
    };
}
