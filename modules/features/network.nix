{ ... }: {
  flake.types.generic.feature-options.network = { inputs, lib }:
    with lib;
    let
      inherit (inputs.self.types) generic;
      simpleFeature = generic.simpleFeature { inherit inputs lib; };
    in simpleFeature "network feature";

  flake.modules.nixos.network-feature = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.network.enable {
      environment.systemPackages = with pkgs; [ gvfs nfs-utils cifs-utils ];

      networking = {
        firewall = {
          allowedTCPPorts = [
            139 # NetBIOS
            445 # SMB
          ];

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
    };
  };
}
