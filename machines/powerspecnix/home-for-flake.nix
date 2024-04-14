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
      barrier
      bat
      byobu
      chromium
      clojure
      curl
      discord
      # distrobox
      doctl
      docker
      # digikam
      # dunst
      earthly
      # emacs
      # extraNodePackages.prettier
      # fish
      gcc9
      gimp
      git
      gitu
      gnumake
      go
      gpa
      gnome.cheese
      # gnome.dconf-editor
      gnome.gnome-tweaks
      gnome-photos
      gnomeExtensions.appindicator
      gnomeExtensions.gsconnect
      # gnomeExtensions.topicons-plus
      gnupg
      # gqrx
      graphviz
      guake
      # helm
      heroic
      hoard
      hstr
      htop
      hydrogen
      # i3
      # itch
      # k3d
      k9s
      keepassxc
      khoj
      # kodi
      # krew
      kubernetes-helm
      kubectl
      kustomize
      mullvad-browser
      neofetch
      # nodejs-16_x
      onlyoffice-bin
      # openjdk
      openjdk17
      openlens
      plex
      plex-media-player
      # nextcloud-24.0.3
      nixfmt-classic
      nixUnstable
      nmap
      # rtl-sdr
      # sdrangel
      silver-searcher
      simplex-chat-desktop
      # slack
      # sparrow
      # steam
      syncthing
      tailscale
      # tailscale-systray
      tdesktop
      # teams
      # thunderbird
      # tilt
      transmission
      tree
      # ungoogled-chromium
      # virtualbox
      vlc
      vscode
      zoom-us
    ];

    sessionPath = [
      "$HOME/.arkade/bin:$PATH"
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

      # bind hstr to Ctrl-r (for Vi mode check doc)
      bindkey -s "\C-r" "\C-a hstr -- \C-j"

      bindkey -s "\C-x\C-tn" "bbg watch-namespaces\C-j"
      bindkey -s "\C-x\C-tp" "bbg watch-pods\C-j"

      source <(k3d completion zsh)
      # source <(devspace completion zsh)

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
