{ config, inputs, pkgs, ... }:
let
  inherit (config) email gpgKey name username;
  hyprland = import ../../programs/hyprland { inherit config inputs pkgs; };
  jujutsu = import ../../programs/jujutsu { inherit config inputs pkgs; };
  zsh = import ../../programs/zsh { inherit config inputs pkgs; };
in {
  home.stateVersion = "21.11";

  programs.home-manager.enable = true;

  imports = [
    # ../../programs/backups
    ../../programs/clojure
    ../../programs/developer
    ../../programs/emacs
    # ../../programs/emacs2
    ../../programs/gaming
    ../../programs/gnome
    hyprland
    # ../../programs/i3
    jujutsu
    # ../../programs/music
    # ../../programs/ncmpcpp
    ../../programs/nostr
    ../../programs/nushell
    # ../../programs/radio
    # ../../programs/vim
    zsh
  ];

  dconf.settings = {
    # "org/gnome/desktop/interface".color-scheme = "prefer-dark";

    "org/gnome/desktop/wm/preferences".button-layout =
      ":minimize,maximize,close";

    "apps/guake/general".default-shell = "/run/current-system/sw/bin/zsh";
  };

  home = {
    username = "${username}";
    homeDirectory = "/home/${username}";

    file.".bb/bb.edn".source = ../../bb.edn;

    packages = with pkgs; [
      aider-chat-full
      alacritty
      appimage-run
      baobab
      bat
      brotab
      byobu
      # chromium
      # cheese
      colmena

      # cloudflare-cli
      # cloudflared

      curl
      digikam
      # discord
      # distrobox
      docker
      # docker-compose
      # dunst
      # earthly
      # emacs
      # fastfetch
      ffmpeg
      # fish
      # gcc9
      gimp
      git
      # gitu
      # gnumeric
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

    sessionPath = [
      "$HOME/.arkade/bin:$PATH"
      "$HOME/.cargo/bin:$PATH"
      "$HOME/.config/yarn/global/node_modules/.bin:$PATH"
      "$HOME/.dotnet:$PATH"
      "$HOME/.local/bin:$PATH"
      "$HOME/.yarn/bin:$PATH"
    ];
  };

  programs = {
    alacritty.enable = true;

    bash = {
      enable = true;
      profileExtra =
        "export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels\${NIX_PATH:+:$NIX_PATH}";
    };

    bat.enable = true;
    btop.enable = true;

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    eza.enable = true;

    firefox.enable = true;

    fish = {
      enable = true;
    };

    git = {
      enable = true;
      userName = "${name}";
      userEmail = "${email}";
      lfs.enable = true;
      signing = {
        signByDefault = false;
        key = gpgKey;
      };
    };

    # gnome-terminal.enable = true;

    gpg.enable = true;
    hstr.enable = true;

    jujutsu = {
      enable = true;
      settings.user = { inherit name email; };
    };

    k9s.enable = true;
    kodi.enable = true;

    mr.enable = true;
    jq.enable = true;
    tmux.enable = true;

    vscode = {
      enable = true;
      profiles.default.userSettings = {
        vs-kubernetes."vs-kubernetes.crd-code-completion" = "enabled";
        nix.serverPath = "nixd";
        "[nix]"."editor.defaultFormatter" = "brettm12345.nixfmt-vscode";
      };
    };
  };

  stylix = {
    enable = true;
    autoEnable = true;
    image = ./nix-wallpaper-mosaic-blue.png;
    # image = config.lib.stylix.pixel "base0A";
    imageScalingMode = "fit";
    polarity = "dark";
    base16Scheme = "${pkgs.base16-schemes}/share/themes/3024.yaml";
    # base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-frappe.yaml";
    # base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-latte.yaml";
    # base16Scheme = "${pkgs.base16-schemes}/share/themes/evenok-dark.yaml";
    # base16Scheme = "${pkgs.base16-schemes}/share/themes/gruvbox-dark-medium.yaml";
    # base16Scheme = "${pkgs.base16-schemes}/share/themes/materia.yaml";
    # base16Scheme = "${pkgs.base16-schemes}/share/themes/onedark-dark.yaml";
    # base16Scheme = "${pkgs.base16-schemes}/share/themes/still-alive.yaml";

    # cursor = {
    #   name = "Bibata-Modern-Ice";
    #   package = pkgs.bibata-cursors;
    # };

    targets.emacs.enable = false;
    targets.firefox.profileNames = ["default"];
    targets.vscode.profileNames = ["default"];

    fonts = {
      # monospace = {
      #   package = pkgs.nerdfonts.override {fonts = ["JetBrainsMono"];};
      #   name = "JetBrainsMono Nerd Font Mono";
      # };
      sansSerif = {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Sans";
      };
      serif = {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Serif";
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
