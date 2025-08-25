{ host, pkgs, ... }:
let inherit (host.identity) username;
in {
  imports = [
    ../backups
    ../clojure
    ../dbt
    ../dconf
    ../developer
    ../dunst
    ../emacs
    ../emacs-prelude
    ../email
    ../flipper
    ../gaming
    ../git
    ../gnome
    ../hyprland
    ../hyprpanel
    ../i3
    ../java
    ../jujutsu
    ../media
    ../music
    ../ncmpcpp
    ../nostr
    ../nushell
    ../office
    ../pictures
    ../radio
    ../stylix
    ../vim
    ../vscode
    ../waybar
    ../zsh
  ];

  home = {
    file.".bb/bb.edn".source = ../../bb.edn;
    homeDirectory = "/home/${username}";

    packages = with pkgs; [
      # aider-chat-full
      appimage-run
      baobab
      bat
      brotab
      byobu
      # chromium
      # cheese
      # cloudflare-cli
      # cloudflared
      code-cursor
      curl
      gnupg
      # gpa
      # gnome.dconf-editor
      # gnome-photos
      # gnome-tweaks
      # gnupg
      # google-chrome
      # graphviz
      # gum
      hstr
      htop
      keepassxc
      lens
      mosh
      neofetch
      nerdfetch
      nh
      nixd
      nixfmt-classic
      # radicle-node
      silver-searcher
      slack
      # simplex-chat-desktop
      youtube-music
    ];

    stateVersion = "21.11";
    username = "${username}";
  };

  programs = {
    alacritty.enable = true;
    bash.enable = true;
    bat.enable = true;
    btop.enable = true;

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    eza.enable = true;
    firefox.enable = true;
    fish.enable = true;
    # gnome-terminal.enable = true;
    gpg.enable = true;
    home-manager.enable = true;
    hstr.enable = true;
    k9s.enable = true;
    kodi.enable = true;
    mr.enable = true;
    jq.enable = true;

    ssh = {
      enable = true;
      extraConfig = ''
        StrictHostKeyChecking=no
      '';
    };

    tmux.enable = true;

    zen-browser = {
      enable = true;
      # find more options here: https://mozilla.github.io/policy-templates/
      policies = {
        DisableAppUpdate = true;
        DisableTelemetry = true;
      };
    };
  };

  targets.genericLinux.enable = true;

  xdg = {
    enable = true;

    # configFile."nix/nix.conf".text = ''
    #   experimental-features = nix-command flakes
    # '';

    mime.enable = true;
  };
}
