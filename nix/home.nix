{ config, pkgs, ... }:

let
  name = "Duck Nebuchadnezzar";
  username = "duck";
  email = "duck@kronkltd.net";
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
      bat
      curl
      emacs
      fish
      git
      gnumake
      helm
      hstr
      htop

      # i3
      keepassxc
      kubectl
      nixfmt
      slack
      # steam
      tdesktop
      tree
      virtualbox
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

  imports = [
    ./programs/ncmpcpp/default.nix
  ];

  # home.file.".emacs/init.el".text = ''
  #   (load "default.el")
  # '';

  programs.direnv = {
    enable = true;

  };

  # programs.emacs = {
  #   enable = true;
  # };

  programs.i3status-rust = {
    enable = true;
  };

  # programs.jq = {
  #   enable = true;
  #   colors = true;
  # };

  programs.git = {
    enable = true;
    userName = "${name}";
    userEmail = "${email}";
    lfs.enable = true;
    signing = {
      signByDefault = true;
      key = "80E3B47F0495EF7E";
    };
  };


  programs.tmux = {
    enable = true;
  };

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
        if [ -e /home/${username}/.nix-profile/etc/profile.d/nix.sh ]; then
            . /home/${username}/.nix-profile/etc/profile.d/nix.sh;
        fi # added by Nix installer
        export PATH="/home/${username}/.local/bin:$PATH"
        export PATH="/home/${username}/.yarn/bin:$PATH"
        export PATH="/home/${username}/.config/yarn/global/node_modules/.bin:$PATH"
        export PATH="/home/${username}/.huber/bin:$PATH"

        bindkey -s "\C-r" "\C-a hstr -- \C-j"     # bind hstr to Ctrl-r (for Vi mode check doc)
        source <(doctl completion zsh)
        source <(k3d completion zsh)
    '';

    localVariables = {
      PROJECT_PATHS = [ ~/projects ];
    };

    sessionVariables = {
      HSTR_CONFIG = "hicolor";
    };

    shellAliases = {
      cat = "bat";
      dkcp = "docker-compose";
      hh = "hstr";
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

    polybar = {
      enable = true;
      config = ./polybar-config;
      script = ''
      for m in $(polybar --list-monitors | ${pkgs.coreutils}/bin/cut -d":" -f1); do
        MONITOR=$m polybar nord &
      done
    '';
    };
  };

  xsession.windowManager.i3 = {
    enable = true;
    config = {
      bars = [];
      # bars = [{
      #   statusCommand = "i3bar";
      # }];
      gaps = {
        inner = 12;
        outer = 5;
        smartBorders = "off";
        smartGaps = true;
      };

      modifier = "Mod4";

      startup = [
        { command = "systemctl --user restart polybar"; always = true; notification = false; }
      ];

      window = {
        hideEdgeBorders = "smart";
      };
    };
  };
}
