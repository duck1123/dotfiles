{ host, lib, pkgs, ... }: {
  config = lib.mkIf host.features.email.enable {
    home.packages = with pkgs; [ thunderbird ];
  };
}
