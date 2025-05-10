{ config, identity, inputs, pkgs, ... }:
let
  inherit (identity) email gpgKey name username;
 
in {
  home.stateVersion = "21.11";

  programs.home-manager.enable = true;

  imports = [
    # ../../programs/backups
    # ../../programs/clojure
    ../../programs/dconf
    # ../../programs/developer
    # ../../programs/emacs
    ../../programs/emacs2
    # ../../programs/gaming
    ../../programs/git
    # ../../programs/gnome
    ../../programs/hyprland
    # ../../programs/i3
    ../../programs/jujutsu
    # ../../programs/music
    # ../../programs/ncmpcpp
    ../../programs/nostr
    ../../programs/nushell
    ../../programs/stylix
    ../../programs/vscode
    # ../../programs/radio
    # ../../programs/vim
    ../../programs/zsh
  ];

  home = {
    username = "${username}";
    homeDirectory = "/home/${username}";
    file.".bb/bb.edn".source = ../../bb.edn;

    packages = with pkgs; [
      appimage-run
      baobab
      bat
      brotab
      byobu
      cheese
      code-cursor
      curl
      gnupg
      guake
      hoard
      hstr
      htop
      keepassxc
      ladybird
      lens
      mosh
      neofetch
      nerdfetch
      nixd
      nixfmt-classic
      nh
      silver-searcher
      youtube-music
    ];

    sessionPath = [ "$HOME/.cargo/bin:$PATH" "$HOME/.local/bin:$PATH" ];
  };

  programs = {
    # alacritty.enable = true;
    bash.enable = true;
    bat.enable = true;
    btop.enable = true;

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    # eza.enable = true;
    firefox.enable = true;
    fish.enable = true;
    # gnome-terminal.enable = true;
    gpg.enable = true;
    hstr.enable = true;
    jq.enable = true;
    k9s.enable = true;
    mr.enable = true;

    ssh = {
      enable = true;
      extraConfig = ''
        StrictHostKeyChecking=no
      '';
    };
  };

  targets.genericLinux.enable = true;

  xdg = {
    enable = true;

    configFile."nix/nix.conf".text = ''
      experimental-features = nix-command flakes
    '';

    mime.enable = true;
  };
}
