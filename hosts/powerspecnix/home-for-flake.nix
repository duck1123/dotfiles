{ identity, pkgs, ... }:
let inherit (identity) username;
in {
  home.stateVersion = "21.11";

  programs.home-manager.enable = true;

  imports = [
    # ../../programs/backups
    ../../programs/clojure
    ../../programs/dbt
    ../../programs/dconf
    ../../programs/developer
    ../../programs/dunst
    ../../programs/emacs
    # ../../programs/emacs2
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
    # ../../programs/radio
    ../../programs/stylix
    # ../../programs/vim
    ../../programs/waybar
    ../../programs/zsh
  ];

  home = {
    username = "${username}";
    homeDirectory = "/home/${username}";

    file.".bb/bb.edn".source = ../../bb.edn;

    packages = with pkgs; [
      # aider-chat-full
      alacritty
      appimage-run
      baobab
      bat
      brotab
      byobu
      # chromium
      # cheese
      code-cursor
      colmena

      # cloudflare-cli
      # cloudflared

      curl
      dbt
      digikam
      # discord
      distrobox
      docker
      # docker-compose
      # earthly
      # fastfetch
      ffmpeg
      # fish
      # gcc9
      gimp
      # gitu
      gnumeric
      gnupg
      # gpa
      # gnome.dconf-editor
      # gnome-photos
      # gnome-tweaks
      # gnupg
      # google-chrome
      # graphviz
      # gum
      hoard
      hstr
      htop
      jdk
      # kakoune
      # kb
      keepassxc
      # keet
      # khoj
      kodi
      kty
      kubernix
      lens
      libnotify
      # logseq
      mdcat
      minio-client
      mosh
      # mullvad-browser
      neofetch
      nerdfetch
      nerd-fonts.fira-code
      networkmanager
      nixfmt-classic
      nix-tree
      nh
      # obsidian
      # onlyoffice-bin
      playerctl
      plex
      # plex-media-player
      # postman
      qFlipper
      # radicle-node
      silver-searcher
      # simplex-chat-desktop
      slack
      # sparrow
      syncthing
      tailscale
      tdesktop
      teams-for-linux
      thunderbird
      # tilt
      transmission_4-gtk
      # tree
      unzip
      # virtualbox
      vlc
      vscode
      wine
      xsel
      youtube-music
      # yt-dlp
      # yq
      # zoom-us
    ];

    sessionPath = [ "$HOME/.cargo/bin:$PATH" "$HOME/.local/bin:$PATH" ];
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
    hstr.enable = true;
    k9s.enable = true;
    kodi.enable = true;
    mr.enable = true;
    jq.enable = true;
    tmux.enable = true;

    vscode = {
      enable = true;
      profiles.default.userSettings = {
        "[nix]"."editor.defaultFormatter" = "brettm12345.nixfmt-vscode";
        "calva.paredit.defaultKeyMap" = "original";
        "direnv.restart.automatic" = true;
        "editor.renderWhitespace" = "trailing";
        "editor.tabSize" = 2;
        "files.autoSave" = "onFocusChange";
        "nix.enableLanguageServer" = true;
        "nix.serverPath" = "nixd";
        "telemetry.feedback.enabled" = false;
        vs-kubernetes."vs-kubernetes.crd-code-completion" = "enabled";
      };
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
