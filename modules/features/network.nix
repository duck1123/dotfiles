{ ... }:
let
  feature-name = "network";
in
{
  flake.types.generic.feature-options.${feature-name} =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "${feature-name} feature";

  flake.modules.homeManager.${feature-name} =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf config.host.features.${feature-name}.enable {
        home.packages = with pkgs; [ networkmanager ];
      };
    };

  flake.modules.nixos.${feature-name} =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf config.host.features.${feature-name}.enable {
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
      };
    };
}
