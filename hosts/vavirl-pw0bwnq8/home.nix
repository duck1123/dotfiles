{ config, pkgs, ... }:
let inherit (config.host.identity) email gpgKey name username;
in {
  programs.home-manager.enable = true;

  imports = [
    ../../programs/clojure
    ../../programs/developer
    # ../../programs/dunst
    ../../programs/emacs-prelude
    ../../programs/jujutsu
    ../../programs/nushell
    ../../programs/zsh
  ];

  home = {
    stateVersion = "21.11";

    # Home Manager needs a bit of information about you and the
    # paths it should manage.
    username = "${username}";
    homeDirectory = "/home/${username}";

    file.".bb/bb.edn".source = ../../bb.edn;

    packages = with pkgs; [
      # apache-airflow
      bat
      direnv
      git
      gitu
      htop
      neofetch
      nixfmt-classic
      nh
      silver-searcher
      sqlcmd
    ];
  };

  programs = {
    bash.enable = true;

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    eza.enable = true;

    git = {
      enable = true;
      lfs.enable = true;

      settings.user = { inherit email name; };

      signing = {
        signByDefault = false;
        key = gpgKey;
      };
    };

    hstr.enable = true;
    jq.enable = true;
    tmux.enable = true;
    # vscode.enable = true;
  };

  # xdg = {
  #   configFile."nix/nix.conf".text = ''
  #     experimental-features = nix-command flakes
  #   '';
  # };
}
