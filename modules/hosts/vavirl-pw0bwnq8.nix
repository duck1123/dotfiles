{ ... }:
let
  hostname = "vavirl-pw0bwnq8";
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
          clojure.enable = true;
          common.enable = true;
          dbt.enable = true;
          dconf.enable = false;
          developer.enable = true;
          docker.enable = true;
          dunst.enable = false;
          emacs.enable = true;
          emacs-prelude.enable = false;
          email.enable = false;
          font.enable = true;
          gaming.enable = false;
          git.enable = true;
          gnome.enable = false;
          hyprland.enable = false;
          i3.enable = false;
          java.enable = true;
          jujutsu.enable = true;

          media = {
            enable = false;
            server.enable = false;
          };

          ncmpcpp.enable = false;
          nix.enable = true;
          nushell.enable = true;
          office.enable = false;
          pictures.enable = false;
          sddm.enable = true;
          sound.enable = true;
          ssh.enable = false;
          starship.enable = true;
          stylix.enable = true;

          syncthing = {
            enable = true;
            shares = {
              camera.enable = false;
              keepass.enable = true;
              org-roam.enable = false;
              renpy.enable = false;
            };
          };

          tailscale.enable = true;
          touch.enable = true;
          vim.enable = false;
          virtualization.enable = false;
          vscode.enable = false;
          waybar.enable = false;
          xserver.enable = false;
          zsh.enable = true;
        };

        id = "TEED77K-QOLTQ37-BL76MFB-LJD46CW-EJ7CZTJ-7GQNEF6-FZAMQRP-BCCRTQ6";
        identity = config.identities.drenfer;
        name = "VallenPC";
        home-manager.enable = true;
        nixos.enable = false;
      };
    };

    homeManager.vavirl-pw0bwnq8 = { config, pkgs, ... }:
      let inherit (config.host.identity) email gpgKey name username;
      in {
        host = config.hosts.${hostname};

        programs.home-manager.enable = true;

        home = {
          stateVersion = "21.11";

          username = "${username}";
          homeDirectory = "/home/${username}";

          file.".bb/bb.edn".source = ../../bb.edn;

          packages = with pkgs; [
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
        };
      };
  };
}
