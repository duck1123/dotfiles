{ ... }:
{
  flake.types.generic.feature-options.vpn =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "vpn feature";

  flake.modules.nixos.vpn =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf config.host.features.vpn.enable {
        boot.kernelModules = [ "wireguard" ];

        environment.systemPackages = with pkgs; [ mullvad-vpn ];

        # Ensure networking is properly configured for WireGuard
        networking.firewall.checkReversePath = "loose";

        services = {
          mullvad-vpn = {
            enable = true;
            # Ensure the daemon can manage WireGuard interfaces
            package = pkgs.mullvad-vpn;
          };
          # Mullvad requires systemd-resolved for DNS management
          resolved.enable = true;
        };
      };
    };
}
