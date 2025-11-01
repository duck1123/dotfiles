{ config, lib, pkgs, ... }: {
  config = lib.mkIf config.host.features.jujutsu.enable {
    home.packages = with pkgs; [
      # gg-jj jj-fzf
      jjui
      jujutsu
      # lazyjj
    ];

    programs.jujutsu = {
      enable = true;
      settings.user = { inherit (config.host.identity) email name; };
    };
  };
}
