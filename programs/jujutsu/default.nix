{ config, pkgs, ... }: {
  home.packages = with pkgs; [
    gg-jj
    jjui
    jujutsu
    # lazyjj
  ];

  programs.jujutsu = {
    enable = true;
    settings.user = { inherit (config) email name; };
  };
}
