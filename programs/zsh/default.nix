{ pkgs, ... }:
let username = "duck";
in {
  home.packages = with pkgs; [ hoard hstr ];

  programs.zsh = {
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

      export PATH="/home/${username}/.cargo/bin:$PATH"
      export PATH="/home/${username}/.local/bin:$PATH"

      # bind hstr to Ctrl-r (for Vi mode check doc)
      bindkey -s "\C-r" "\C-a hstr -- \C-j"

      source <(argo completion zsh)
      source <(argocd completion zsh)
      source <(k3d completion zsh)
      source <(devspace completion zsh)
      source <(runme completion zsh)
      source <(hoard shell-config --shell zsh)

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
      SHELL = "/run/current-system/sw/bin/zsh";
    };

    shellAliases = {
      d = "devspace";
      dr = "devspace run";
      cat = "bat";
      hh = "hstr";
      bbg = "bb --config ~/.bb/bb.edn";
      psgrep = "ps -ef | grep -v grep | grep ";
      rmr = "runme run";
    };
  };
}
