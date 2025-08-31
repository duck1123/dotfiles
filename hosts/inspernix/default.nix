{ system, identities, ... }: {
  inherit system;
  id = "OWMQLRL-CD5VB7H-A3T436E-6XT4H66-6XRF22Y-MQXMNAU-DFRNGOV-ADSKFAV";
  identity = identities.duck;
  name = "inspernix";
  hostname = "inspernix";

  features = {
    backups.enable = false;
    bitcoin.enable = false;
    chm.enable = false;
    clojure.enable = true;
    dbt.enable = false;
    dconf.enable = false;
    developer.enable = false;
    dunst.enable = false;
    emacs.enable = true;
    emacs-prelude.enable = false;
    email.enable = false;
    gaming.enable = true;
    git.enable = true;
    gnome.enable = true;
    flipper.enable = false;
    hyprland.enable = true;
    hyprpanel.enable = true;
    i3.enable = false;
    java.enable = true;
    jujutsu.enable = true;

    kubernetes = {
      client.enable = true;
      server.enable = false;
    };

    media = {
      enable = false;
      server.enable = false;
    };

    music.enable = false;
    ncmpcpp.enable = false;
    nfs.enable = true;
    nostr.enable = true;
    nushell.enable = true;
    office.enable = false;
    pictures.enable = false;
    radio.enable = false;
    sddm.enable = true;
    stylix.enable = true;

    syncthing = {
      enable = true;

      shares = {
        camera.enable = false;
        keepass.enable = true;
        org-roam.enable = true;
        renpy.enable = true;
      };
    };

    vim.enable = false;
    virtualization.enable = false;
    vscode.enable = true;
    waybar.enable = false;
    zsh.enable = true;
  };

  nixos = {
    enable = true;
    budgie.enable = false;
    gnome.enable = false;
    hyprland.enable = false;
    i3.enable = false;
    plasma6.enable = false;
  };
}
