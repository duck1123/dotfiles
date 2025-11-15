{ ... }: {
  flake.modules.homeManager.backups = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.backups.enable {
      home.packages = with pkgs; [ borgmatic deja-dup duplicati restic ];
    };
  };
}

