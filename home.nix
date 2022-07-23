{ config, pkgs, ... }:

let
  name = "Duck Nebuchadnezzar";
  username = "duck";
  email = "duck@kronkltd.net";
  gpgKey = "80E3B47F0495EF7E";
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
      arkade
      babashka
      barrier
      bat
      byobu
      chromium
      curl
      # doctl
      # docker
      # dunst
      # earthly
      # emacs
      # fish
      git
      gnumake
      graphviz
      guake
      # helm
      hstr
      htop
      # i3
      keepassxc
      # k3d
      # kodi
      kubectl
      # nextcloud-24.0.3
      nixfmt
      nixUnstable
      silver-searcher
      slack
      # steam
      tdesktop
      tree
      # virtualbox
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
      signByDefault = false;
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
      export PATH="/home/${username}/.arkade/bin:$PATH"
      export PATH="/home/${username}/.local/bin:$PATH"
      export PATH="/home/${username}/.yarn/bin:$PATH"
      export PATH="/home/${username}/.config/yarn/global/node_modules/.bin:$PATH"

      bindkey -s "\C-r" "\C-a hstr -- \C-j"     # bind hstr to Ctrl-r (for Vi mode check doc)

      bindkey -s "\C-x\C-td" "tilt down --delete-namespaces\C-j"
      bindkey -s "\C-x\C-tn" "bbg watch-namespaces\C-j"
      bindkey -s "\C-x\C-tp" "bbg watch-pods\C-j"
      bindkey -s "\C-x\C-tu" "tilt up --legacy=true\C-j"

      source <(k3d completion zsh)
      source <(arkade completion zsh)
      eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"

      _bb_tasks() {
        local matches=(`bb tasks |tail -n +3 |cut -f1 -d ' '`)
        compadd -a matches
        _files # autocomplete filenames as well
      }
      compdef _bb_tasks bb
    '';

    localVariables = { PROJECT_PATHS = [ ~/projects ]; };

    sessionVariables = {
      EDITOR = "emacsclient -ct";
      HSTR_CONFIG = "hicolor";
    };

    shellAliases = {
      cat = "bat";
      dkcp = "docker-compose";
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

  xdg.configFile."nix/nix.conf".text = ''
    experimental-features = nix-command flakes
  '';
}
