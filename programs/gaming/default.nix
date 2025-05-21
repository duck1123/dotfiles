{ pkgs, ... }: {
  home.packages = with pkgs; [
    heroic

    itch

    # Open Source gaming platform for GNU/Linux
    lutris

    # nexusmods-app
    protontricks
  ];
}
