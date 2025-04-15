{ config, inputs, pkgs, ... }:
let
  inherit (config) email gpgKey name username;
  jujutsu = import ../../programs/jujutsu { inherit config inputs pkgs; };
  zsh = import ../../programs/zsh { inherit config inputs pkgs; };
in {
  programs.home-manager.enable = true;

  imports = [
    ../../programs/clojure
    ../../programs/developer
    ../../programs/emacs
    jujutsu
    ../../programs/nushell
    zsh
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
      barrier
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
    bash = {
      enable = true;
      profileExtra =
        "export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels\${NIX_PATH:+:$NIX_PATH}";
    };

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    eza.enable = true;

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

    hstr.enable = true;
    jq.enable = true;
    tmux.enable = true;
    # vscode.enable = true;
  };

  xdg = {
    configFile."nix/nix.conf".text = ''
      experimental-features = nix-command flakes
    '';
  };
}
