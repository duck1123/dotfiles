{ pkgs, ... }: {
  imports = [
    # ../../programs/backups
    ../../programs/base
    ../../programs/clojure
    ../../programs/dconf
    # ../../programs/dunst
    ../../programs/developer
    # ../../programs/emacs
    ../../programs/emacs2
    ../../programs/gaming
    ../../programs/git
    ../../programs/gnome
    ../../programs/hyprland
    ../../programs/hyprpanel
    # ../../programs/i3
    ../../programs/jujutsu
    # ../../programs/music
    # ../../programs/ncmpcpp
    ../../programs/nostr
    ../../programs/nushell
    ../../programs/stylix
    ../../programs/vscode
    ../../programs/radio
    # ../../programs/vim
    ../../programs/waybar
    ../../programs/zsh
  ];

  home = {
    packages = with pkgs; [ cheese discord ladybird nerdfetch nixd ];
    sessionPath = [ "$HOME/.cargo/bin:$PATH" "$HOME/.local/bin:$PATH" ];
  };
}
