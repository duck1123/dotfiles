{ ... }:
{
  flake.types.generic.feature-options.common =
    { inputs, lib }:
    let
      inherit (inputs.self.types.generic) simpleFeature;
    in
    simpleFeature { inherit inputs lib; } "common feature";

  flake.modules.homeManager.common =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      inherit (config.host.identity) username;
    in
    {
      config = lib.mkIf config.host.features.common.enable {
        home = {
          homeDirectory = "/home/${username}";

          packages = with pkgs; [
            # aider-chat-full
            appimage-run
            argocd
            baobab
            bat
            brotab
            byobu
            # chromium
            # cheese
            # cloudflare-cli
            # cloudflared
            # code-cursor
            curl
            gnupg
            # gpa
            # gnome.dconf-editor
            # gnome-photos
            # gnome-tweaks
            # gnupg
            # google-chrome
            # graphviz
            # gum
            # fastfetch
            hstr
            htop
            keepassxc
            lens
            libnotify
            # mosh
            nerdfetch
            nh
            nix-tree
            nixd
            nixfmt
            # pear-desktop
            # radicle-node
            silver-searcher
            # slack
            # simplex-chat-desktop
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

        targets.genericLinux.enable = true;

        xdg = {
          enable = true;
          mime.enable = true;
        };
      };
    };
}
