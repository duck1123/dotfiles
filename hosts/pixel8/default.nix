{ system, identities, ... }: {
  inherit system;
  android.enable = true;

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
    font.enable = false;
    gaming.enable = false;
    git.enable = false;
    gnome.enable = true;
    hyprland.enable = false;
    hyprpanel.enable = true;
    i3.enable = false;
    java.enable = false;
    jujutsu.enable = false;

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
    nfs.enable = true;
    nix.enable = true;
    nostr.enable = true;
    nushell.enable = true;
    office.enable = false;
    pictures.enable = false;
    radio.enable = false;
    sddm.enable = true;
    stylix.enable = false;

    syncthing = {
      enable = true;

      shares = {
        camera.enable = true;
        keepass.enable = true;
        org-roam.enable = true;
        renpy.enable = false;
      };
    };

    vim.enable = false;
    virtualization.enable = false;
    vscode.enable = true;
    waybar.enable = false;
    zsh.enable = true;
  };

  hostname = "pixel8";
  id = "7Y3NTUQ-MRUHGO4-5L34ZC7-EDRXHKA-QVCG7AJ-HWHIINY-OV5B2T7-OFQS2QP";
  identity = identities.duck;
  name = "Pixel 8";
}
