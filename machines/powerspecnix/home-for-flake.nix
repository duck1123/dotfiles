{ inputs, config, pkgs, stylix, ... }:

let
  name = "Duck Nebuchadnezzar";
  username = "duck";
  email = "duck@kronkltd.net";
  gpgKey = "9564904D297DBF3C";
in {
  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.11";

  programs.home-manager.enable = true;

  imports = [
    # ../../programs/backups
    ../../programs/clojure
    ../../programs/developer
    # ../../programs/emacs
    ../../programs/emacs2
    # ../../programs/i3
    # ../../programs/music
    # ../../programs/ncmpcpp
    ../../programs/nostr
    # ../../programs/radio
    ../../programs/zsh
  ];

  dconf.settings = {
    "org/gnome/desktop/interface".color-scheme = "prefer-dark";

    "org/gnome/desktop/wm/preferences".button-layout =
      ":minimize,maximize,close";

    "apps/guake/general".default-shell = "/run/current-system/sw/bin/zsh";
  };

  home = {
    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    username = "${username}";
    homeDirectory = "/home/${username}";

    file.".bb/bb.edn".source = ../../bb.edn;

    packages = with pkgs; [
      alacritty

      appimage-run

      barrier
      bat

      byobu

      # chromium

      # cheese

      curl

      digikam

      # discord
      # distrobox

      docker
      # docker-compose

      # dunst
      # earthly

      # emacs

      ffmpeg

      # fish
      # gcc9
      # gimp

      git

      # gitu

      # Modern release of the GNU Privacy Guard, a GPL OpenPGP implementation
      gnupg

      go
      # gossip

      # gpa

      # gnome.dconf-editor

      # gnome-photos
      # gnome-tweaks

      gnomeExtensions.appindicator
      gnomeExtensions.gsconnect
      # gnomeExtensions.topicons-plus
      # gnomeExtensions.tailscale-status

      # gnupg

      # graphviz
      guake

      # gum

      heroic
      hoard
      hstr
      htop

      # itch

      jdk

      jet

      # Minimalist command line knowledge base manager
      # kb

      keepassxc

      # Peer-to-Peer Chat
      # keet

      # khoj
      kodi

      libnotify

      # DAW similar to FL Studio (music production software)
      # lmms

      # A local-first, non-linear, outliner notebook for organizing and sharing your personal knowledge base
      # logseq

      # Open Source gaming platform for GNU/Linux
      lutris

      # mullvad-browser

      neofetch
      # nextcloud-24.0.3

      # nexusmods-app

      nixfmt-classic

      nh

      nmap

      nodejs

      # obs-studio

      # obsidian

      # onlyoffice-bin
      plex
      # plex-media-player

      # postman

      # qFlipper

      # A decentralized app for code collaboration
      # radicle-node

      silver-searcher
      # simplex-chat-desktop
      slack
      # sparrow

      syncthing

      tailscale

      tdesktop

      teams-for-linux

      # thunderbird

      # tilt

      # transmission_4-gtk

      # tree

      unzip

      vlc

      vscode

      wine

      xsel

      # youtube-music

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
    bash = {
      enable = true;
      profileExtra =
        "export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels\${NIX_PATH:+:$NIX_PATH}";
    };

    direnv = {
      enable = true;
      nix-direnv.enable = true;
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

    gpg.enable = true;

    jq.enable = true;

    tmux.enable = true;

    vim = {
      enable = true;
      extraConfig = ''
        syntax on
        " Wrap gitcommit file types at the appropriate length
        filetype indent plugin on
      '';
    };

    vscode.enable = true;
  };

  stylix = {
    enable = true;
    autoEnable = true;
    image = ./nix-wallpaper-mosaic-blue.png;
    # image = config.lib.stylix.pixel "base0A";
    imageScalingMode = "fit";
    polarity = "dark";
    # base16Scheme = "${pkgs.base16-schemes}/share/themes/3024.yaml";
    base16Scheme = "${pkgs.base16-schemes}/share/themes/materia.yaml";
    # base16Scheme = "${pkgs.base16-schemes}/share/themes/evenok-dark.yaml";
    # base16Scheme = "${pkgs.base16-schemes}/share/themes/onedark-dark.yaml";
    # base16Scheme = "${pkgs.base16-schemes}/share/themes/gruvbox-dark-medium.yaml";
    # base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-latte.yaml";
    # base16Scheme = "${pkgs.base16-schemes}/share/themes/still-alive.yaml";

    cursor = {
      name = "Bibata-Modern-Ice";
      package = pkgs.bibata-cursors;
    };

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
