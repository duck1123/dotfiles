{ config, pkgs, ... }:

let
  name = "Duck Nebuchadnezzar";
  username = "duck";
  email = "duck@kronkltd.net";
  gpgKey = "9564904D297DBF3C";
  extraNodePackages = import ./node/default.nix {};
in {
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  nixpkgs.config.allowUnfree = true;

  home = {
    stateVersion = "21.05";

    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    username = "${username}";
    homeDirectory = "/home/${username}";

    packages = with pkgs; [
      babashka
      # barrier
      bat
      byobu
      # chromium
      curl
      # doctl
      # docker
      # digikam
      # dunst
      # earthly
      # emacs
      # extraNodePackages.prettier
      # fish
      git
      gnumake
      go
      gpa
      graphviz
      guake
      # helm
      hstr
      htop
      # i3
      keepassxc
      # k3d
      k9s
      # kodi
      krew
      kubectl
      kustomize
      # nodejs-16_x
      # openjdk
      openjdk17
      plex-media-player
      # plex-media-server
      # plex-mpv-shim
      # plexmediaserver
      # plexmediaserver
      # nextcloud-24.0.3
      nixfmt
      nixUnstable
      nmap
      silver-searcher
      slack
      # sparrow
      # steam
      tdesktop
      # teams
      # thunderbird
      # tilt
      # transmission
      tree
      # virtualbox
      vlc
    ];

    # sessionPath = [
    #   "~/.dotnet/tools"
    #   "~/.cargo/bin"
    #   "~/.config/yarn/global/node_modules/.bin"
    #   "~/.dotfiles/bin"
    #   "~/.local/bin"
    #   "~/.huber/bin"
    #   "~/.nix-profile/bin"
    #   "~/.yarn/bin"
    # ];
  };

  imports = [
    ./programs/emacs/default.nix
    ./programs/i3/default.nix
    ./programs/ncmpcpp/default.nix
  ];

  home.file.".bb/bb.edn".source = ./bb.edn;

  programs.direnv = {
    enable = true;
  };

  programs.jq = {
    enable = true;
    # colors = true;
  };

  programs.git = {
    enable = true;
    userName = "${name}";
    userEmail = "${email}";
    lfs.enable = true;
    signing = {
      signByDefault = true;
      key = gpgKey;
    };
  };

  programs.tmux = { enable = true; };

  programs.vim = {
    enable = true;
    extraConfig = ''
      syntax on
      " Wrap gitcommit file types at the appropriate length
      filetype indent plugin on
    '';
  };

  programs.bash = {
    enable = true;
    profileExtra = "export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels$\{NIX_PATH:+:$NIX_PATH\}";
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    defaultKeymap = "emacs";

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
        "command-not-found"
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


    # export PATH="/home/${username}/.pulumi/bin:$PATH"
    # export PATH="/home/${username}/.huber/bin:$PATH"

    initExtra = ''
      if [ -e /home/${username}/.nix-profile/etc/profile.d/nix.sh ]; then
          . /home/${username}/.nix-profile/etc/profile.d/nix.sh;
      fi # added by Nix installer
      export PATH="/home/${username}/.local/bin:$PATH"
      export PATH="/home/${username}/.dotnet:$PATH"
      export PATH="/home/${username}/.yarn/bin:$PATH"
      export PATH="/home/${username}/.config/yarn/global/node_modules/.bin:$PATH"

      bindkey -s "\C-r" "\C-a hstr -- \C-j"     # bind hstr to Ctrl-r (for Vi mode check doc)

      bindkey -s "\C-x\C-td" "tilt down --delete-namespaces\C-j"
      bindkey -s "\C-x\C-tn" "bbg watch-namespaces\C-j"
      bindkey -s "\C-x\C-tp" "bbg watch-pods\C-j"
      bindkey -s "\C-x\C-tu" "tilt up --legacy=true\C-j"

      source <(k3d completion zsh)
      source <(devspace completion zsh)

      _bb_tasks() {
        local matches=(`bb tasks |tail -n +3 |cut -f1 -d ' '`)
        compadd -a matches
      }
      compdef _bb_tasks bb
    '';

    # _files # autocomplete filenames as well

    localVariables = { PROJECT_PATHS = [ ~/projects ]; };

    sessionVariables = {
      EDITOR = "emacsclient -ct";
      HSTR_CONFIG = "hicolor";
    };

    shellAliases = {
      d = "devspace";
      dr = "devspace run";
      cat = "bat";
      # dkcp = "docker-compose";
      hh = "hstr";
      bbg = "bb --config ~/.bb/bb.edn";
      psgrep = "ps -ef | grep -v grep | grep ";
      "reload!" = "home-manager switch && . ~/.zshrc";
    };
  };

  services = {
    # gnome3.gnome-keyring.enable = true;

    # dbus = {
    #   enable = true;
    #   socketActivated = true;
    #   packages = [ pkgs.gnome3.dconf ];
    # };
  };

  xdg = {
    enable = true;
    mime = {
      enable = true;
    };
  };

  targets.genericLinux = {
    enable = true;
  };

  xdg.configFile."nix/nix.conf".text = ''
    experimental-features = nix-command flakes
  '';
}
