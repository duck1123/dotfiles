{ ... }:
{
  flake.modules.nixos.environments-hyprland =
    { pkgs, ... }:
    {
      environment = {
        sessionVariables = {
          NIXOS_OZONE_WL = "1";
          WLR_NO_HARDWARE_CURSORS = "1";
        };

        systemPackages = with pkgs; [
          file-roller
          pyprland
          hyprcursor
          hyprlock
          hypridle
          starship
          tuigreet
        ];
      };

      programs.hyprland.enable = true;
      services.displayManager.defaultSession = "hyprland";
    };
}
