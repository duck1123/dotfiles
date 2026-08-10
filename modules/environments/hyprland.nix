{ ... }:
{
  flake.modules.nixos.environments-hyprland =
    {
      config,
      inputs,
      pkgs,
      ...
    }:
    let
      system = pkgs.stdenv.hostPlatform.system;
      inherit (config.host.identity) username;
    in
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
          zathura
        ];
      };

      # Enable Hyprland
      programs.hyprland = {
        enable = true;
        # set the flake package
        package = inputs.hyprland.packages.${system}.hyprland;
        # make sure to also set the portal package, so that they are in sync
        portalPackage = inputs.hyprland.packages.${system}.xdg-desktop-portal-hyprland;
      };

      # Force the session to load our home-manager-managed hyprland.lua
      # explicitly, rather than relying on Hyprland's default config
      # auto-search (which regenerates its own example hyprland.lua on
      # every launch and can shadow ours).
      services.displayManager.sessionPackages = [
        (pkgs.writeTextFile {
          name = "hyprland-explicit-config-session";
          destination = "/share/wayland-sessions/hyprland-explicit-config.desktop";
          text = ''
            [Desktop Entry]
            Name=Hyprland
            Comment=An intelligent dynamic tiling Wayland compositor
            Exec=start-hyprland -- --config /home/${username}/.config/hypr/hyprland.lua
            Type=Application
            DesktopNames=Hyprland
          '';
          passthru.providedSessions = [ "hyprland-explicit-config" ];
        })
      ];

      services.displayManager.defaultSession = "hyprland-explicit-config";
    };
}
