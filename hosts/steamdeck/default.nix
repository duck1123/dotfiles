{ system, identities, ... }: {
  inherit system;

  features = {
    backups.enable = false;
    bitcoin.enable = false;
    chm.enable = false;
    clojure.enable = false;
    dbt.enable = false;
    dconf.enable = false;
    developer.enable = false;
    dunst.enable = false;
    emacs.enable = false;
    emacs-prelude.enable = false;
    email.enable = false;
    flipper.enable = false;
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
    nostr.enable = false;
    nushell.enable = false;
    office.enable = false;
    pictures.enable = false;
    radio.enable = false;
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

    vim.enable = false;
    virtualization.enable = false;
    vscode.enable = false;
    waybar.enable = false;
    zsh.enable = false;
  };

  hostname = "steamdeck";
  id = "ZPO3QWJ-LQHVWBH-TAI3LLD-ZS6WSBM-N5IQ7JX-P4HUVF3-XNOX6N4-NBIF3AX";
  identity = identities.deck;
  name = "steamdeck";
  home-manager.enable = true;
  nixos.enable = false;
}
