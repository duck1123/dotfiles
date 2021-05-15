{ config, pkgs, ... }:

let
  username = "duck";
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home = {
    stateVersion = "21.05";

    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    username = "${username}";
    homeDirectory = "/home/${username}";

    packages = with pkgs; [
      curl
      fish
      emacs
      helm
      nixfmt
      tdesktop
    ];

    # sessionPath = [
    #   "~/.dotnet/tools"
    #   "~/.cargo/bin"
    #   "~/.config/yarn/global/node_modules/.bin"
    #   "~/.dotfiles-old/bin"
    #   "~/.local/bin"
    #   "~/.huber/bin"
    #   "~/.nix-profile/bin"
    #   "~/.yarn/bin"
    # ];
  };

  programs.direnv = {
    enable = true;

  };

  programs.git = {
    enable = true;
    userName = "Duck Nebuchadnezzar";
    userEmail = "duck@kronkltd.net";
    # signing = {
    #   signByDefault = true;
    #   key = "";
    # };
  };

  programs.zsh = {
    enable = true;
    defaultKeymap = "emacs";
    history = {
      extended = true;
    };

    oh-my-zsh = {
      enable = true;
      theme = "jonathan";
      plugins = [
        "bgnotify"
        "colorize"
        "command-not-found"
        "compleat"
        "docker-compose"
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
        "zsh_reload"
      ];
    };

    plugins = [
      # {
      #   name = "bb-task-completion";
      #   src = "~/projects/bb-task-completion";
      # }
      {
        name = "bb-task-completion";
        src = pkgs.fetchFromGitHub {
          owner = "duck1123";
          repo = "bb-task-completion";
          rev = "0.0.1";
          sha256 = "04gvnd0kngy057ia1w9s52yjbkb8vnpv811p7cqfsqpac9ici19b";
        };
      }
    ];

    initExtra = ''
        if [ -e /home/duck/.nix-profile/etc/profile.d/nix.sh ]; then
            . /home/duck/.nix-profile/etc/profile.d/nix.sh;
        fi # added by Nix installer
        export PATH="~/.yarn/bin:~/.config/yarn/global/node_modules/.bin:$PATH"
        export PATH="~/.huber/bin:$PATH"

        # export HSTR_CONFIG=hicolor       # get more colors
        bindkey -s "\C-r" "\C-a hstr -- \C-j"     # bind hstr to Ctrl-r (for Vi mode check doc)
        source <(doctl completion zsh)
        source <(k3d completion zsh)
    '';

    # initExtraFirst = ''
    #     echo a
    #     echo b
    # '';

    localVariables = {
      PROJECT_PATHS = [ ~/projects ];
      # fpath = [
      #   \$fpath
      #   "~/projects/bb-task-completion/"
      # ];
    };

    shellAliases = {
      "reload!" = ". ~/.zshrc";
      psgrep = "ps -ef | grep -v grep | grep ";
      dkcp = "docker-compose";
      hh = "hstr";
    };

    loginExtra = ''
      echo "Running login extra"
    '';

    enableAutosuggestions = true;
#     profileExtra = ''
# emulate sh
# if [ -f /etc/profile ] && [ ! -v __ETC_PROFILE_DONE ]; then
#   . /etc/profile
# fi
# if [ -f ~/.profile ]; then
#   . ~/.profile
# fi
# emulate zsh
# if [[ -f "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" ]]; then
#   source "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
# fi
# '';
  };
}
