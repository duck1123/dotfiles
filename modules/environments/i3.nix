_: {
  environments.i3.nixos =
    { pkgs, ... }:
    {
      services.xserver.windowManager.i3 = {
        enable = true;
        package = pkgs.i3-gaps;
      };
    };
}
