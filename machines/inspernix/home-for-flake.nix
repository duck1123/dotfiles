{ config, inputs, pkgs, ... }:
let
  inherit (config) email gpgKey name username;
  git = import ../../programs/git { inherit config inputs pkgs; };
  hyprland = import ../../programs/hyprland { inherit config inputs pkgs; };
  jujutsu = import ../../programs/jujutsu { inherit config inputs pkgs; };
  zsh = import ../../programs/zsh { inherit config inputs pkgs; };
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
    git
    # ../../programs/gnome
    hyprland
    # ../../programs/i3
    jujutsu
    # ../../programs/music
    # ../../programs/ncmpcpp
    ../../programs/nostr
    ../../programs/nushell
    ../../programs/stylix
    ../../programs/vscode
    # ../../programs/radio
    # ../../programs/vim
    zsh
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
      curl
      gnupg
      guake
      hoard
      hstr
      htop
      keepassxc
      lens
      neofetch
      nerdfetch
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
