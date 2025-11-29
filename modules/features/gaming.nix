{ ... }: {
  flake.modules.homeManager.gaming = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.gaming.enable {
      home.packages = with pkgs; [
        # heroic
        itch
        lutris
        # nexusmods-app
        protontricks
        satisfactorymodmanager
      ];
    };
  };

  flake.modules.nixos.gaming-feature = { config, lib, ... }: {
    config = lib.mkIf config.host.features.gaming.enable {
      programs.steam.enable = true;
    };
  };
}
