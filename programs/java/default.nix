{ host, lib, pkgs, ... }: {
  options = {
    features.java.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable java";
    };
  };

  config =
    lib.mkIf host.features.java.enable { home.packages = with pkgs; [ jdk ]; };
}
