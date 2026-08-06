{ ... }:
{
  flake.types.generic.feature-options.waydroid =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "waydroid feature";

  flake.modules.nixos.waydroid-feature =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf config.host.features.waydroid.enable {
        # Enable clipboard sharing
        environment.systemPackages = with pkgs; [
          waydroid-helper
          wl-clipboard
        ];

        services.geoclue2.enable = true;

        systemd = {
          packages = with pkgs; [ waydroid-helper ];
          services.waydroid-mount.wantedBy = [ "multi-user.target" ];
        };

        virtualisation.waydroid = {
          enable = true;
          # Newer kernel versions may need
          package = pkgs.waydroid-nftables;
        };
      };
    };
}
