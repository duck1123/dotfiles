{ host, lib, ... }: {
  options = {
    features.nfs.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable nfs";
    };
  };

  config = lib.mkIf host.features.nfs.enable {
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
}
