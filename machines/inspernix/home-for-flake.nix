{ config, inputs, pkgs, ... }:
let
  inherit (config) email gpgKey name username;
  hyprland = import ../../programs/hyprland { inherit config inputs pkgs; };
  jujutsu = import ../../programs/jujutsu { inherit config inputs pkgs; };
  zsh = import ../../programs/zsh { inherit config inputs pkgs; };
in {
  home.stateVersion = "21.11";

  programs.home-manager.enable = true;

  imports = [
    # ../../programs/backups
    # ../../programs/clojure
    # ../../programs/developer
    ../../programs/emacs
    # ../../programs/emacs2
    # ../../programs/gaming
    # ../../programs/gnome
    hyprland
    # ../../programs/i3
    jujutsu
    # ../../programs/music
    # ../../programs/ncmpcpp
    ../../programs/nostr
    ../../programs/nushell
    # ../../programs/radio
    # ../../programs/vim
    zsh
  ];

  dconf.settings = {
    "org/gnome/desktop/interface".color-scheme = "prefer-dark";

    "org/gnome/desktop/wm/preferences".button-layout =
      ":minimize,maximize,close";

    # "apps/guake/general".default-shell = "/run/current-system/sw/bin/zsh";
  };

  home = {
    username = "${username}";
    homeDirectory = "/home/${username}";

    file.".bb/bb.edn".source = ../../bb.edn;

    packages = with pkgs; [
      appimage-run
      baobab
      bat
      brotab
      byobu
      cheese
      curl
      emacs
      git
      gnupg
      guake
      hoard
      hstr
      htop
      keepassxc
      lens
      neofetch
      nerdfetch
      nixfmt-classic
      nh
      silver-searcher
      youtube-music
    ];

    sessionPath = [ "$HOME/.cargo/bin:$PATH" "$HOME/.local/bin:$PATH" ];
  };

  programs = {
    # alacritty.enable = true;
    bash.enable = true;
    bat.enable = true;
    btop.enable = true;

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    # eza.enable = true;

    firefox.enable = true;

    fish.enable = true;

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

    # gnome-terminal.enable = true;

    gpg.enable = true;
    hstr.enable = true;
    jq.enable = true;

    jujutsu = {
      enable = true;
      settings.user = { inherit name email; };
    };

    k9s.enable = true;

    mr.enable = true;

    ssh = {
      enable = true;
      extraConfig = ''
        StrictHostKeyChecking=no
      '';
    };

    vscode = {
      enable = true;
      profiles.default.userSettings = {
        "[nix]"."editor.defaultFormatter" = "brettm12345.nixfmt-vscode";
        "calva.paredit.defaultKeyMap" = "original";
        "direnv.restart.automatic" = true;
        "editor.renderWhitespace" = "trailing";
        "editor.tabSize" = 2;
        "files.autoSave" = "onFocusChange";
        "nix.enableLanguageServer" = true;
        "nix.serverPath" = "nixd";
        "telemetry.feedback.enabled" = false;
        vs-kubernetes."vs-kubernetes.crd-code-completion" = "enabled";
      };
    };
  };

  stylix = {
    enable = true;
    autoEnable = true;
    image = ../powerspecnix/nix-wallpaper-mosaic-blue.png;
    imageScalingMode = "fit";
    polarity = "dark";
    base16Scheme = "${pkgs.base16-schemes}/share/themes/3024.yaml";

    targets.emacs.enable = false;
    targets.firefox.profileNames = [ "default" ];
    targets.vscode.profileNames = [ "default" ];

    fonts = {
      # monospace = {
      #   package = pkgs.nerdfonts.override {fonts = ["JetBrainsMono"];};
      #   name = "JetBrainsMono Nerd Font Mono";
      # };
      sansSerif = {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Sans";
      };
      serif = {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Serif";
      };
    };
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
