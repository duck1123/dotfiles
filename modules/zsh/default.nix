{ host, lib, pkgs, ... }: {
  config = lib.mkIf host.features.zsh.enable {
    environment.systemPackages = with pkgs; [ zsh ];
    programs.zsh.enable = true;
  };
}
