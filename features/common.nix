{
  homeManager =
    { config, pkgs, ... }:
    let
      inherit (config.host.identity) username;
    in
    {
      home = {
        homeDirectory = "/home/${username}";

        packages = with pkgs; [
          appimage-run
          argocd
          baobab
          bat
          brotab
          byobu
          curl
          gnupg
          hstr
          htop
          keepassxc
          lens
          libnotify
          nerdfetch
          nh
          nix-tree
          nixd
          nixfmt
          ripgrep
          smartmontools
          unzip
          xsel
        ];

        username = "${username}";
      };

      programs = {
        alacritty.enable = false;
        bash.enable = true;
        bat.enable = true;
        btop.enable = true;

        direnv = {
          enable = true;
          nix-direnv.enable = true;
        };

        eza.enable = false;

        fish.enable = true;
        # gnome-terminal.enable = true;
        gpg.enable = true;
        home-manager.enable = true;
        hstr.enable = true;
        k9s.enable = true;
        mr.enable = false;
        jq.enable = true;

        tmux.enable = true;
      };

      targets.genericLinux.enable = !config.host.nixos.enable;

      xdg = {
        enable = true;
        mime.enable = true;
      };
    };
}
