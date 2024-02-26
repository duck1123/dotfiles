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
      arkade
      babashka
      # barrier
      bat
      byobu
      # chromium
      curl
      # doctl
      docker
      # digikam
      # dunst
      # earthly
      # emacs
      # extraNodePackages.prettier
      fish
      gcc9
      git
      gnumake
      go
      gpa
      gnome.dconf-editor
      gnupg
      graphviz
      guake
      # helm
      hstr
      htop
      # i3
      itch
      keepassxc
      # k3d
      k9s
      # kodi
      krew
      kubectl
      kustomize
      neofetch
      # nodejs-16_x
      # openjdk
      openjdk17
      openlens
      plex
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
      syncthing
      tdesktop
      # teams
      # thunderbird
      # tilt
      # transmission
      tree
      virtualbox
      vlc
    ];
  };

  programs.bash = {
    enable = true;
    profileExtra =
      "export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels\${NIX_PATH:+:$NIX_PATH}";
  };

  programs.direnv = { enable = true; };

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

  programs.jq = {
    enable = true;
    # colors = true;
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

    # export PATH="/home/${username}/.pulumi/bin:$PATH"
    # export PATH="/home/${username}/.huber/bin:$PATH"

    initExtra = ''
      if [ -e /home/${username}/.nix-profile/etc/profile.d/nix.sh ]; then
          . /home/${username}/.nix-profile/etc/profile.d/nix.sh;
      fi # added by Nix installer

      export PATH="/home/${username}/.arkade/bin:$PATH"
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
      # source <(devspace completion zsh)

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

  dconf.settings = {
    "org/gnome/desktop/wm/preferences".button-layout =
      ":minimize,maximize,close";

    "apps/guake/general".default-shell = "/run/current-system/sw/bin/zsh";
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

    configFile."nix/nix.conf".text = ''
      experimental-features = nix-command flakes
    '';

    mime.enable = true;
  };

  targets.genericLinux = { enable = true; };
}
