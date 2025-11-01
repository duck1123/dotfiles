{ config, lib, pkgs, ... }: {
  config = lib.mkIf config.host.features.tailscale.enable {
    services.tailscale.enable = true;
    # Disable tests to avoid test failures in 1.86.4
    services.tailscale.package =
      pkgs.tailscale.overrideAttrs (oldAttrs: { doCheck = false; });
  };
}
