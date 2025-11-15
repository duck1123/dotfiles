{ ... }: {
  flake.modules.nixos.nfs-feature = { config, lib, ... }: {
    config = lib.mkIf config.host.features.nfs.enable {
      services.nfs.server = {
        enable = false;
        statdPort = 4000;
        lockdPort = 4001;
        mountdPort = 4002;

        exports = ''
          /mnt/nfs    *(ro,insecure,all_squash)
        '';
      };
    };
  };
}

