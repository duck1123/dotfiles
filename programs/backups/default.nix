{ pkgs, ... }: {
  home.packages = with pkgs; [ borgmatic deja-dup duplicati restic ];
}
