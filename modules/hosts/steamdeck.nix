{ ... }:
let
  hostname = "steamdeck";
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
            battery.enable = true;
            bluetooth.enable = true;
            common.enable = true;
            emacs.enable = true;
            font.enable = true;
            gaming.enable = false;
            git.enable = true;
            gnome.enable = false;
            hyprland.enable = false;
            hyprpanel.enable = false;
            i3.enable = false;
            java.enable = false;
            jujutsu.enable = false;

            nix.enable = true;
            nushell.enable = true;
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
            zsh.enable = false;
          };

          id = "ZPO3QWJ-LQHVWBH-TAI3LLD-ZS6WSBM-N5IQ7JX-P4HUVF3-XNOX6N4-NBIF3AX";
          identity = config.identities.deck;
          name = hostname;
          home-manager.enable = true;
          nixos.enable = false;
        };
      };

    homeManager.${hostname} =
      { config, ... }:
      {
        host = config.hosts.${hostname};
      };
  };
}
