{ pkgs, ... }:

let
  name = "Duck Nebuchadnezzar";
  username = "deck";
  email = "duck@kronkltd.net";
  gpgKey = "9564904D297DBF3C";
in {
  home.stateVersion = "21.11";

  home = {
    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    username = "${username}";
    homeDirectory = "/home/${username}";

    file.".bb/bb.edn".source = ../../bb.edn;

    packages = with pkgs; [
      appimage-run
      babashka
      bat
      devspace
      digikam
      emacs
      firefox
      git
      gitu
      gnumake
      guake
      hstr
      htop
      jet
      keepassxc
      neofetch
      nixfmt-classic
      syncthing
      tailscale
      vscode
      wine
    ];
  };

  programs = {
    bash.enable = true;
    direnv.enable = true;

    git = {
      enable = true;
      lfs.enable = true;

      settings.user = { inherit email name; };

      signing = {
        signByDefault = false;
        key = gpgKey;
      };
    };

    gpg.enable = true;
    jq.enable = true;
    home-manager.enable = true;
    tmux.enable = true;

    zsh = {
      autosuggestion.enable = true;

      defaultKeymap = "emacs";
      enable = true;

      history = {
        expireDuplicatesFirst = true;
        extended = true;
        ignoreAllDups = true;
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

      initContent = ''
        if [ -e /home/${username}/.nix-profile/etc/profile.d/nix.sh ]; then
            . /home/${username}/.nix-profile/etc/profile.d/nix.sh;
        fi # added by Nix installer

        # bind hstr to Ctrl-r (for Vi mode check doc)
        bindkey -s "\C-r" "\C-a hstr -- \C-j"

        # source <(argocd completion zsh)
        # source <(k3d completion zsh)
        # source <(devspace completion zsh)

        _bb_tasks() {
          local matches=(`bb tasks |tail -n +3 |cut -f1 -d ' '`)
          compadd -a matches
        }
        compdef _bb_tasks bb
      '';

      localVariables.PROJECT_PATHS = [ "/home/${username}/projects" ];

      sessionVariables = {
        EDITOR = "emacsclient -ct";
        HSTR_CONFIG = "hicolor";
      };

      shellAliases = {
        d = "devspace";
        dr = "devspace run";
        cat = "bat";
        hh = "hstr";
        bbg = "bb --config ~/.bb/bb.edn";
        psgrep = "ps -ef | grep -v grep | grep ";
        "reload!" = "bbg switch-home && . ~/.zshrc";
      };
    };
  };
}
