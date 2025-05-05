{ ... }: {
  services.nfs.server = {
    enable = false;
    statdPort = 4000;
    lockdPort = 4001;
    mountdPort = 4002;

    exports = ''
      /mnt/nfs    *(ro,insecure,all_squash)
    '';
  };
}
