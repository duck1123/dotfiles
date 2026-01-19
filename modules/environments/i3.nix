{ ... }:
{
  flake.modules.nixos.environments-i3 =
    { pkgs, ... }:
    {
      services.xserver.windowManager.i3 = {
        enable = true;
        package = pkgs.i3-gaps;
      };
    };
}
