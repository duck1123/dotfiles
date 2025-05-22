{ pkgs, ... }: {
  imports = [
    # ../../programs/backups
    ../../programs/base
    ../../programs/clojure
    # ../../programs/dbt
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
    ../../programs/vscode
    ../../programs/waybar
    ../../programs/zsh
  ];

  home = {
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
}
