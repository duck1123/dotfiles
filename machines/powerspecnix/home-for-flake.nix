{ inputs, config, pkgs, ... }:

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
    ../../programs/emacs/default.nix
    ../../programs/i3/default.nix
    ../../programs/ncmpcpp/default.nix
  ];

  home = {
    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    username = "${username}";
    homeDirectory = "/home/${username}";

    file.".bb/bb.edn".source = ../../bb.edn;

    packages = with pkgs; [
      appimage-run

      argocd
      arkade

      # Next generation multi-platform command line experience for Azure
      # azure-cli

      babashka
      barrier
      bat
      byobu
      chromium
      clojure
      clojure-lsp
      cheese
      curl


      # Universal SQL Client for developers, DBA and analysts. Supports MySQL, PostgreSQL, MariaDB, SQLite, and more
      dbeaver-bin

      devpod
      devpod-desktop

      devspace
      # discord
      # distrobox

      # A command line tool for DigitalOcean services
      doctl

      docker
      docker-compose

      digikam

      # dunst
      earthly

      emacs

      # extraNodePackages.prettier
      # fish
      gcc9
      gimp
      git
      # gitu
      gnumake
      go
      # gossip
      gpa

      # gnome.dconf-editor

      gnome-photos
      gnome-tweaks

      gnomeExtensions.appindicator
      gnomeExtensions.gsconnect
      # gnomeExtensions.topicons-plus
      gnupg
      gqrx
      graphviz
      guake
      gum

      heroic
      hoard
      hstr
      htop

      # Advanced drum machine
      hydrogen

      # i3
      # itch

      jdk22

      jet

      # k3d
      k9s
      keepassxc

      # Peer-to-Peer Chat
      # keet

      # khoj
      # kodi
      # krew
      kubernetes-helm
      kubectl
      kustomize

      lens

      libnotify

      # A local-first, non-linear, outliner notebook for organizing and sharing your personal knowledge base
      # logseq

      lutris

      mullvad-browser

      # neofetch
      # nextcloud-24.0.3

      # nexusmods-app

      nixfmt-classic
      # nixUnstable

      nmap

      nodejs
      # nodejs-16_x

      # obs-studio

      # obsidian

      onlyoffice-bin
      # openjdk

      # openjdk17
      plex
      plex-media-player

      podman
      podman-desktop
      podman-tui

      python3

      # qFlipper

      # A decentralized app for code collaboration
      # radicle-node

      # A backup program that is fast, efficient and secure
      # restic

      # rtl-sdr

      runme

      # sdrangel

      silver-searcher
      simplex-chat-desktop
      slack
      # sparrow

      # Command line tool for querying Sybase/MSSQL databases
      # sqsh

      # steam
      syncthing
      tailscale
      # tailscale-systray
      tdesktop
      teams-for-linux

      thunderbird
      # tilt
      transmission_4-gtk
      tree

      # ungoogled-chromium
      unzip

      # virtualbox
      vlc
      vscode

      wine

      youtube-music

      yq

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

    direnv.enable = true;

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

    jq.enable = true;

    # kdeconnect.enable = true;

    tmux.enable = true;

    vim = {
      enable = true;
      extraConfig = ''
      syntax on
      " Wrap gitcommit file types at the appropriate length
      filetype indent plugin on
      '';
    };

    vscode = {
      enable = true;
    };
  };

  programs.zsh = {
    autosuggestion.enable = true;

    defaultKeymap = "emacs";
    enable = true;

    history = {
      expireDuplicatesFirst = true;
      extended = true;
      ignoreDups = true;
    };

    oh-my-zsh = {
      enable = true;
      theme = "jonathan";
      plugins = [
        "bgnotify"
        "colorize"
        # "command-not-found"
        "compleat"
        # "docker-compose"
        "docker"
        "git"
        "git-extras"
        "history"
        "kubectl"
        "nmap"
        "node"
        "npm"
        "pj"
        "sudo"
        "systemd"
      ];
    };

    initExtra = ''
      if [ -e /home/${username}/.nix-profile/etc/profile.d/nix.sh ]; then
          . /home/${username}/.nix-profile/etc/profile.d/nix.sh;
      fi # added by Nix installer

      export PATH="/home/${username}/.arkade/bin:$PATH"
      export PATH="/home/${username}/.cargo/bin:$PATH"
      export PATH="/home/${username}/.local/bin:$PATH"

      # bind hstr to Ctrl-r (for Vi mode check doc)
      bindkey -s "\C-r" "\C-a hstr -- \C-j"

      bindkey -s "\C-x\C-tn" "bbg watch-namespaces\C-j"
      bindkey -s "\C-x\C-tp" "bbg watch-pods\C-j"

      source <(argocd completion zsh)
      source <(k3d completion zsh)
      source <(devspace completion zsh)
      source <(hoard shell-config --shell zsh)

      _bb_tasks() {
        local matches=(`bb tasks |tail -n +3 |cut -f1 -d ' '`)
        compadd -a matches
      }
      compdef _bb_tasks bb
    '';

    localVariables.PROJECT_PATHS = [ ~/projects ];

    sessionVariables = {
      EDITOR = "emacsclient -ct";
      HSTR_CONFIG = "hicolor";
      SHELL = "/run/current-system/sw/bin/zsh";
    };

    shellAliases = {
      d = "devspace";
      dr = "devspace run";
      cat = "bat";
      # dkcp = "docker-compose";
      hh = "hstr";
      bbg = "bb --config ~/.bb/bb.edn";
      psgrep = "ps -ef | grep -v grep | grep ";
      "reload!" = "bbg switch-home && . ~/.zshrc";
    };
  };

  dconf.settings = {
    "org/gnome/desktop/interface".color-scheme = "prefer-dark";

    "org/gnome/desktop/wm/preferences".button-layout =
      ":minimize,maximize,close";

    "apps/guake/general".default-shell = "/run/current-system/sw/bin/zsh";
  };

  services = {
    # barrier = {
    #   enable = true;
    #   # client = {
    #   #   enable = true;
    #   # };
    # };

    # gnome3.gnome-keyring.enable = true;

    # dbus = {
    #   enable = true;
    #   socketActivated = true;
    #   packages = [ pkgs.gnome3.dconf ];
    # };

    # openvscode-server = {
    #   enable = true;
    # };
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
