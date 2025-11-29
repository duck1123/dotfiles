{ ... }: {
  flake.types.generic.feature-options.zsh = { inputs, lib }:
    let inherit (inputs.self.types.generic) simpleFeature;
    in simpleFeature { inherit inputs lib; } "zsh feature";

  flake.modules.homeManager.zsh = { config, lib, pkgs, ... }: {
    config = let inherit (config.host.identity) username;
    in lib.mkIf config.host.features.zsh.enable {
      home.packages = with pkgs; [ hstr ];

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

        initContent = ''
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
          # SHELL = "/run/current-system/sw/bin/zsh";
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
    };
  };

  flake.modules.nixos.zsh-feature = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.host.features.zsh.enable {
      environment.systemPackages = with pkgs; [ zsh ];
      programs.zsh.enable = true;
    };
  };
}
