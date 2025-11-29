{ ... }:
let
  hostname = "steamdeck";
  system = "x86_64-linux";
in {
  flake.modules = {
    generic.${hostname} = { config, ... }: {
      hosts.${hostname} = {
        inherit hostname system;

        features = {
          backups.enable = false;
          battery.enable = true;
          bitcoin.enable = false;
          bluetooth.enable = true;
          chm.enable = false;
          clojure.enable = false;
          common.enable = true;
          dbt.enable = false;
          dconf.enable = false;
          developer.enable = false;
          docker.enable = false;
          dunst.enable = false;
          emacs.enable = false;
          emacs-prelude.enable = false;
          email.enable = false;
          font.enable = true;
          gaming.enable = false;
          git.enable = true;
          gnome.enable = true;
          hyprland.enable = false;
          hyprpanel.enable = true;
          i3.enable = false;
          java.enable = true;
          jujutsu.enable = true;

          kubernetes = {
            client.enable = false;
            server.enable = false;
          };

          media = {
            enable = false;
            server.enable = false;
          };

          music.enable = false;
          ncmpcpp.enable = false;
          nfs.enable = false;
          nix.enable = true;
          nostr.enable = false;
          nushell.enable = false;
          office.enable = false;
          pictures.enable = false;
          radio.enable = false;
          sddm.enable = true;
          sound.enable = true;
          ssh.enable = false;
          starship.enable = true;
          stylix.enable = false;

          syncthing = {
            enable = true;
            shares = {
              camera.enable = false;
              keepass.enable = true;
              org-roam.enable = false;
              renpy.enable = true;
            };
          };

          tailscale.enable = true;
          touch.enable = true;
          vim.enable = false;
          virtualization.enable = false;
          vscode.enable = false;
          waybar.enable = false;
          xserver.enable = true;
          zsh.enable = false;
        };

        id = "ZPO3QWJ-LQHVWBH-TAI3LLD-ZS6WSBM-N5IQ7JX-P4HUVF3-XNOX6N4-NBIF3AX";
        identity = config.identities.deck;
        name = hostname;
        home-manager.enable = true;
        nixos.enable = false;
      };
    };

    homeManager.${hostname} = { config, pkgs, ... }:
      let inherit (config.host.identity) email gpgKey name username;
      in {
        host = config.hosts.${hostname};

        home.stateVersion = "21.11";

        home = {
          # Home Manager needs a bit of information about you and the
          # paths it should manage.
          username = "${username}";
          homeDirectory = "/home/${username}";

          packages = with pkgs; [
            appimage-run
            babashka
            bat
            devspace
            digikam
            emacs
            firefox
            git
            gitu
            gnumake
            guake
            hstr
            htop
            jet
            keepassxc
            neofetch
            nixfmt-classic
            syncthing
            tailscale
            vscode
            wine
          ];
        };

        programs = {
          bash.enable = true;
          direnv.enable = true;

          git = {
            enable = true;
            lfs.enable = true;

            settings.user = { inherit email name; };

            signing = {
              signByDefault = false;
              key = gpgKey;
            };
          };

          gpg.enable = true;
          jq.enable = true;
          home-manager.enable = true;
          tmux.enable = true;

          zsh = {
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

              # bind hstr to Ctrl-r (for Vi mode check doc)
              bindkey -s "\C-r" "\C-a hstr -- \C-j"

              # source <(argocd completion zsh)
              # source <(k3d completion zsh)
              # source <(devspace completion zsh)

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
            };

            shellAliases = {
              d = "devspace";
              dr = "devspace run";
              cat = "bat";
              hh = "hstr";
              bbg = "bb --config ~/.bb/bb.edn";
              psgrep = "ps -ef | grep -v grep | grep ";
              "reload!" = "bbg switch-home && . ~/.zshrc";
            };
          };
        };
      };
  };
}
