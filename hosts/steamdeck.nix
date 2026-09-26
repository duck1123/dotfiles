{ config, ... }:
{
  system = "x86_64-linux";

  features = {
    battery.enable = true;
    bluetooth.enable = true;
    common.enable = true;
    emacs.enable = true;
    firefox.enable = true;
    font.enable = true;
    gaming.enable = false;
    git.enable = true;
    gnome.enable = false;
    hyprland.enable = false;
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
    zen-browser.enable = true;
    zsh.enable = false;
  };

  id = "ZPO3QWJ-LQHVWBH-TAI3LLD-ZS6WSBM-N5IQ7JX-P4HUVF3-XNOX6N4-NBIF3AX";
  identity = config.identities.deck;
  home-manager.enable = true;
  nixos.enable = false;

  # no extra home-manager config, but this still generates homeConfigurations
  modules.homeManager = { };
}
