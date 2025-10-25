{ host, lib, pkgs, ... }: {
  options = {
    features.tailscale.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable tailscale";
    };
  };

  config = lib.mkIf host.features.tailscale.enable {
    services.tailscale.enable = true;
    # Disable tests to avoid test failures in 1.86.4
    services.tailscale.package =
      pkgs.tailscale.overrideAttrs (oldAttrs: { doCheck = false; });
  };
}
