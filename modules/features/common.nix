{ ... }: {
  flake.types.generic.feature-options.common = { inputs, lib }:
    let inherit (inputs.self.types.generic) simpleFeature;
    in simpleFeature { inherit inputs lib; } "common feature";

  flake.modules.homeManager.common = { config, lib, pkgs, ... }:
    let inherit (config.host.identity) username;
    in {
      config = lib.mkIf config.host.features.common.enable {
        home = {
          file.".bb/bb.edn".source = ../../bb.edn;
          homeDirectory = "/home/${username}";

          packages = with pkgs; [
            # aider-chat-full
            appimage-run
            baobab
            bat
            brotab
            byobu
            # chromium
            # cheese
            # cloudflare-cli
            # cloudflared
            code-cursor
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
            hstr
            htop
            keepassxc
            lens
            libnotify
            mosh
            neofetch
            nerdfetch
            nh
            nix-tree
            nixd
            nixfmt-classic
            pear-desktop
            # radicle-node
            silver-searcher
            slack
            # simplex-chat-desktop
            unzip
            xsel
          ];

          stateVersion = "21.11";
          username = "${username}";
        };

        programs = {
          alacritty.enable = true;
          bash.enable = true;
          bat.enable = true;
          btop.enable = true;

          direnv = {
            enable = true;
            nix-direnv.enable = true;
          };

          eza.enable = true;
          firefox.enable = true;
          fish.enable = true;
          # gnome-terminal.enable = true;
          gpg.enable = true;
          home-manager.enable = true;
          hstr.enable = true;
          k9s.enable = true;
          mr.enable = true;
          jq.enable = true;

          tmux.enable = true;

          zen-browser = {
            enable = true;
            # find more options here: https://mozilla.github.io/policy-templates/
            policies = {
              DisableAppUpdate = true;
              DisableTelemetry = true;
            };
          };
        };

        targets.genericLinux.enable = true;

        xdg = {
          enable = true;
          mime.enable = true;
        };
      };
    };
}
