{ host, lib, pkgs, ... }: {
  config =
    lib.mkIf host.features.java.enable { home.packages = with pkgs; [ jdk ]; };
}
