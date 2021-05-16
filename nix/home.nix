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

  programs.ncmpcpp = {
    enable = true;
    settings = {
      # Playlist
      now_playing_prefix = "$b";
      now_playing_suffix = "$/b";
      playlist_display_mode = "columns (classic/columns)";
      autocenter_mode = "yes";
      centered_cursor = "yes";
      visualizer_fifo_path = "/tmp/mpd.fifo";
      visualizer_output_name = "my_fifo";
      visualizer_sync_interval = "30";
      visualizer_in_stereo = "yes";
      # visualizer_type = "wave" (spectrum/wave)
      visualizer_type = "spectrum (spectrum/wave)";
      visualizer_color = "green";
      visualizer_look = "∙▋";
      # progressbar_look = "━■"
      progressbar_look = "▀▀ ";
      progressbar_color = "black";
      progressbar_elapsed_color = "blue";
      # Bars
      song_list_format = " $1%a $5//$8 %t";
      # song_list_format = "$1• $8%t $1by $2%a$2 $R$1%l";
      # song_list_format = "$8 %t $R$8";
      song_status_format = " $2%a $4⟫$3⟫ $8%t $4⟫$3⟫ $5%b ";
      # song_status_format = "%t » %a »{ %b » }%y";
      # progressbar_look = "|] ";
      titles_visibility = "no";
      song_columns_list_format = "(6f)[default]{l} (40)[default]{t|f} (25)[default]{a} (30)[default]{b}";
      # Browser
      browser_playlist_prefix = "$2plist »$9 ";
      browser_display_mode = "columns (classic/columns)";
      mouse_support = "yes";
      header_visibility = "no";
      statusbar_visibility = "yes";
      enable_window_title = "no";
      # Colors
      discard_colors_if_item_is_selected = "yes";
      header_window_color = "default";
      volume_color = "default";
      state_line_color = "default";
      state_flags_color = "default";
      main_window_color = "default";
      color1 = "default";
      color2 = "default";
      main_window_highlight_color = "white";
      statusbar_color = "default";
      active_column_color = "default";
      # Others
      song_window_title_format = "{%a - }{%t}{ - %b{ Disc %d}}|{%f}";
      search_engine_display_mode = "columns (classic/columns)";
      follow_now_playing_lyrics = "yes";
      display_screens_numbers_on_start = "no";
      clock_display_seconds = "yes";
      # execute_on_song_change = "twmnc -c \"$(ncmpcpp --now-playing %a' - '%t)\";
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
