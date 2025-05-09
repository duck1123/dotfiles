{ identity, pkgs, ... }: {
  home.packages = with pkgs; [ gg-jj jj-fzf jjui jujutsu lazyjj ];

  programs.jujutsu = {
    enable = true;
    settings.user = { inherit (identity) email name; };
  };
}
