{ system, identities, ... }: {
  inherit system;

  features = {
    backups.enable = false;
    bitcoin.enable = false;
    chm.enable = false;
    clojure.enable = true;
    dbt.enable = false;
    dconf.enable = false;
    developer.enable = true;
    dunst.enable = false;
    emacs.enable = true;
    emacs-prelude.enable = false;
    email.enable = true;
    flipper.enable = false;
    font.enable = true;
    gaming.enable = true;
    git.enable = true;
    gnome.enable = true;
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
      enable = true;
      server.enable = false;
    };

    music.enable = false;
    ncmpcpp.enable = false;
    network.enable = true;
    nfs.enable = true;
    nix.enable = true;
    nostr.enable = true;
    nushell.enable = true;
    office.enable = true;
    pictures.enable = true;
    radio.enable = false;
    sddm.enable = true;
    stylix.enable = true;

    syncthing = {
      enable = true;

      shares = {
        camera.enable = true;
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

  hostname = "powerspecnix";
  id = "UFCCQLJ-3EKBVCQ-O5CNVM5-ERJQAQG-JWKQRPU-7FOZHPG-VMEOMKJ-KZSUFQK";
  identity = identities.duck;
  name = "powerspecnix";
  nixos.enable = true;
}
