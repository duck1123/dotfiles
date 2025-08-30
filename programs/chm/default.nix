{ host, lib, pkgs, ... }: {
  config = lib.mkIf host.features.chm.enable {
    home.packages = with pkgs; [
      kchmviewer
    ];
  };
}
