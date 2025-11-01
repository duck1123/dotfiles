{ config, lib, pkgs, ... }: {
  config = lib.mkIf config.host.features.zsh.enable {
    environment.systemPackages = with pkgs; [ zsh ];
    programs.zsh.enable = true;
  };
}
