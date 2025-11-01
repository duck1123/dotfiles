{ config, lib, pkgs, ... }: {
  config = lib.mkIf config.host.features.chm.enable {
    home.packages = with pkgs; [ kchmviewer ];
  };
}
