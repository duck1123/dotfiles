{ host, lib, pkgs, ... }: {
  options = {
    features.email.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable email";
    };
  };

  config = lib.mkIf host.features.email.enable {
    home.packages = with pkgs; [ thunderbird ];
  };
}
