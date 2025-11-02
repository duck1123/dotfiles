{ config, lib, pkgs, ... }: {
  config = lib.mkIf config.host.features.java.enable {
    home.packages = with pkgs; [ jdk ];
  };
}
