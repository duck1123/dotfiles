_:
let
  hostname = "vavirl-pw0bwnq8";
  system = "x86_64-linux";
in
{
  flake.modules = {
    generic.${hostname} =
      { config, ... }:
      {
        hosts.${hostname} = {
          inherit hostname system;

          features = {
            backups.enable = false;
            battery.enable = true;
            bitcoin.enable = false;
            bluetooth.enable = true;
            clojure.enable = true;
            common.enable = true;
            dbt.enable = true;
            developer.enable = true;
            docker.enable = true;
            emacs.enable = true;
            email.enable = false;
            firefox.enable = false;
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
              enable = false;
              shares = {
                camera.enable = false;
                keepass.enable = true;
                org-roam.enable = false;
                renpy.enable = false;
              };
            };

            tailscale.enable = false;
            touch.enable = true;
            vim.enable = false;
            virtualization.enable = false;
            vscode.enable = false;
            waybar.enable = false;
            xserver.enable = false;
            zen-browser.enable = false;
            zsh.enable = true;
          };

          id = "TEED77K-QOLTQ37-BL76MFB-LJD46CW-EJ7CZTJ-7GQNEF6-FZAMQRP-BCCRTQ6";
          identity = config.identities.drenfer;
          name = "VallenPC";
          home-manager.enable = true;
          nixos.enable = true;
        };
      };

    nixos.${hostname} =
      { config, ... }:
      {
        host = config.hosts.${hostname};
        time.timeZone = "America/Detroit";
        wsl.defaultUser = config.hosts.${hostname}.identity.username;
      };

    homeManager.vavirl-pw0bwnq8 =
      { config, pkgs, ... }:
      let
        inherit (config.host.identity)
          email
          gpgKey
          name
          username
          ;
      in
      {
        host = config.hosts.${hostname};

        programs.home-manager.enable = true;

        home = {
          username = "${username}";
          homeDirectory = "/home/${username}";
          sessionPath = [ "$HOME/.local/bin" ];

          packages = with pkgs; [
            bat
            direnv
            git
            htop
            nh
            ripgrep
            sqlcmd
          ];
        };

        dconf.enable = false;

        programs = {
          bash.enable = true;

          direnv = {
            enable = true;
            nix-direnv.enable = true;
          };

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
