{ inputs, config, pkgs, ... }:

let
  name = "Daniel E. Renfer";
  username = "drenfer";
  email = "drenfer@vallen.com";
  gpgKey = "9564904D297DBF3C";
in {
  programs.home-manager.enable = true;

  imports = [
    ../../programs/emacs/default.nix
  ];

  home = {
    stateVersion = "21.11";

    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    username = "${username}";
    homeDirectory = "/home/${username}";

    file.".bb/bb.edn".source = ../../bb.edn;

    packages = with pkgs; [
      babashka
      barrier
      bat
      clojure
      devspace
      direnv
      emacs
      git
      gitu
      gnumake
      hoard
      hstr
      htop
      jet
      neofetch
      nixfmt-classic
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

    tmux.enable = true;
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
        "compleat"
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

      # source <(argocd completion zsh)
      # source <(k3d completion zsh)
      # source <(devspace completion zsh)
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
    };

    shellAliases = {
      d = "devspace";
      dr = "devspace run";
      cat = "bat";
      hh = "hstr";
      bbg = "bb --config ~/.bb/bb.edn";
      psgrep = "ps -ef | grep -v grep | grep ";
    };
  };
}
