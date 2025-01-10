{ pkgs, ... }: {
  home.packages = with pkgs; [
    borgmatic
    deja-dup

    duplicati

    # A backup program that is fast, efficient and secure
    restic
  ];
}
