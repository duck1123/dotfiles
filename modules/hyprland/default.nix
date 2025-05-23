{ inputs, pkgs, ... }:
let system = pkgs.stdenv.hostPlatform.system;
in {
  environment = {
    sessionVariables = {
      NIXOS_OZONE_WL = "1";
      WLR_NO_HARDWARE_CURSORS = "1";
    };

    systemPackages = with pkgs; [
      pyprland
      hyprpicker
      hyprcursor
      hyprlock
      hypridle
      hyprpaper
      starship
      helix
      # qutebrowser
      greetd.tuigreet
      zathura
      mpv
      imv
    ];
  };

  # Enable Hyprland
  programs.hyprland = {
    enable = true;
    # set the flake package
    package = inputs.hyprland.packages.${system}.hyprland;
    # make sure to also set the portal package, so that they are in sync
    portalPackage =
      inputs.hyprland.packages.${system}.xdg-desktop-portal-hyprland;
  };

  services.displayManager.defaultSession = "hyprland";
}
