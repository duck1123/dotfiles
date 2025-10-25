{ host, lib, pkgs, ... }: {
  options = {
    features.backups.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable backups";
    };
  };

  config = lib.mkIf host.features.backups.enable {
    home.packages = with pkgs; [ borgmatic deja-dup duplicati restic ];
  };
}
