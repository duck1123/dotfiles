_: {
  flake.modules.nixos.environments-niri =
    { pkgs, ... }:
    {
      environment = {
        sessionVariables.NIXOS_OZONE_WL = "1";

        # niri's generated default config binds Mod+T to alacritty and
        # Mod+D to fuzzel, and spawns xwayland-satellite on demand for X11
        # apps when it's on PATH.
        systemPackages = with pkgs; [
          alacritty
          fuzzel
          xwayland-satellite
        ];
      };

      programs.niri.enable = true;

      services.displayManager.defaultSession = "niri";
    };
}
