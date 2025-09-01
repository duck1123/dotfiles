{ host, lib, pkgs, ... }: {
  config = lib.mkIf host.features.tailscale.enable {
    services.tailscale.enable = true;
  };
}
