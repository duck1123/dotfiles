_: {
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

        # Ensure networking is properly configured for WireGuard
        networking.firewall.checkReversePath = "loose";

        services = {
          mullvad-vpn = {
            enable = true;
            gui.enable = true;
          };
          # Mullvad requires systemd-resolved for DNS management
          resolved.enable = true;
        };

        # Mullvad blocks all LAN access while connected unless "local network
        # sharing" is turned on; that setting lives in the daemon's own state
        # (not NixOS config), so enforce it declaratively on every boot.
        systemd.services.mullvad-lan-allow = {
          description = "Allow LAN access through Mullvad (local network sharing)";
          after = [ "mullvad-daemon.service" ];
          requires = [ "mullvad-daemon.service" ];
          wantedBy = [ "multi-user.target" ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            ExecStart = "${lib.getExe' pkgs.mullvad "mullvad"} lan set allow";
            Restart = "on-failure";
            RestartSec = 2;
          };
        };
      };
    };
}
